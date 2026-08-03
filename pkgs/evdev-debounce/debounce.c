/*
 * evdev-debounce -- an interception-tools filter that suppresses phantom
 * button events produced by a worn microswitch.
 *
 * Reads struct input_event from stdin, writes to stdout. Intended to sit
 * between interception-tools' `intercept -g` and `uinput -d`.
 *
 * The device this was written for (Logitech Marble, 046d:c408) chatters on
 * the *release* edge: a legitimate 0.5-2.5s hold ends, and 16-32ms later the
 * contact briefly re-closes, which applications read as a new click. That
 * collapses text selections and dismisses context menus.
 *
 * So: never emit a button release immediately. Withhold it for a window. If a
 * press of the same code arrives inside that window, the release was chatter --
 * the button was never really up -- so drop both and carry on as if the hold
 * had continued. Otherwise emit the release when the window expires.
 *
 * Only releases are delayed. Motion passes through untouched, so there is no
 * added pointer latency, and press timestamps are never altered -- press-to-press
 * intervals, which is what double-click detection uses, survive intact. The cost
 * is `window` milliseconds of latency on every button release.
 *
 * The default window is derived from measurement, not taste. Release-to-press
 * gaps in three libinput recordings of this device were sharply bimodal:
 *
 *   phantoms:  8 8 8 8 8 8 8 8 8 16 16 16 16 16 24 32 32 40 40 40
 *              <-- nothing whatsoever between 40 and 72 -->
 *   genuine:   72 72 88 88 88 96 112 120 136 144 160 200 ...
 *
 * The phantom cluster sits on multiples of 8ms, the device's USB polling
 * interval, which is what a sampled contact bounce looks like. 56ms is the
 * midpoint of the empty band: it swallows every observed phantom and still
 * leaves 16ms of margin before the fastest genuine click.
 *
 * Usage: evdev-debounce [window_ms]   (default 56)
 */

#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <linux/input.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#define NCODES (KEY_MAX + 1)

static long window_us = 56000;

static int held[NCODES];                  /* a release is being withheld    */
static long long due_us[NCODES];          /* monotonic deadline for it      */
static struct input_event withheld[NCODES];

static struct input_event scan;           /* stashed MSC_SCAN, see below    */
static int have_scan;

static long long
mono_us(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (long long)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}

static void
stamp_now(struct input_event *e)
{
	/* Re-stamp on the way out: a withheld release must never carry a
	 * timestamp older than events already emitted after it. */
	struct timeval tv;
	gettimeofday(&tv, NULL);
	e->input_event_sec = tv.tv_sec;
	e->input_event_usec = tv.tv_usec;
}

static void
out(const struct input_event *e)
{
	if (fwrite(e, sizeof *e, 1, stdout) != 1)
		exit(1);
}

static void
flush_due(long long now)
{
	for (int c = 0; c < NCODES; c++) {
		if (!held[c] || now < due_us[c])
			continue;
		held[c] = 0;

		struct input_event ev = withheld[c];
		stamp_now(&ev);
		out(&ev);

		struct input_event syn;
		memset(&syn, 0, sizeof syn);
		syn.type = EV_SYN;
		syn.code = SYN_REPORT;
		stamp_now(&syn);
		out(&syn);
	}
}

static int
read_event(struct input_event *e)
{
	unsigned char *p = (unsigned char *)e;
	size_t got = 0;

	while (got < sizeof *e) {
		ssize_t n = read(STDIN_FILENO, p + got, sizeof *e - got);
		if (n == 0)
			return 0;
		if (n < 0) {
			if (errno == EINTR)
				continue;
			return -1;
		}
		got += (size_t)n;
	}
	return 1;
}

int
main(int argc, char **argv)
{
	if (argc > 1) {
		long ms = strtol(argv[1], NULL, 10);
		if (ms > 0 && ms < 1000)
			window_us = ms * 1000;
	}

	/* Unbuffered: every event must reach uinput as soon as it is decided. */
	setvbuf(stdout, NULL, _IONBF, 0);

	struct pollfd pfd = { .fd = STDIN_FILENO, .events = POLLIN };

	for (;;) {
		long long now = mono_us();

		long long nearest = -1;
		for (int c = 0; c < NCODES; c++)
			if (held[c] && (nearest < 0 || due_us[c] < nearest))
				nearest = due_us[c];

		int timeout = -1;
		if (nearest >= 0) {
			long long d = (nearest - now + 999) / 1000;
			timeout = d < 0 ? 0 : (int)d;
		}

		int r = poll(&pfd, 1, timeout);
		if (r < 0) {
			if (errno == EINTR)
				continue;
			break;
		}
		if (r == 0) {
			flush_due(mono_us());
			continue;
		}

		struct input_event ev;
		if (read_event(&ev) <= 0)
			break;

		now = mono_us();
		flush_due(now);

		/* MSC_SCAN precedes its key event in the same frame. Hold it back
		 * one event so a dropped key does not leave an orphan scancode. */
		if (ev.type == EV_MSC && ev.code == MSC_SCAN) {
			scan = ev;
			have_scan = 1;
			continue;
		}

		if (ev.type == EV_KEY && ev.value != 2) {
			int c = ev.code;

			if (ev.value == 0) {            /* release: withhold it   */
				withheld[c] = ev;
				held[c] = 1;
				due_us[c] = now + window_us;
				have_scan = 0;
				continue;
			}

			if (held[c]) {                  /* press inside window:   */
				held[c] = 0;            /* it never was released  */
				have_scan = 0;
				continue;
			}
		}

		if (have_scan) {
			out(&scan);
			have_scan = 0;
		}
		out(&ev);
	}

	/* Never leave a button stuck down for whoever reads the uinput device. */
	flush_due(mono_us() + (long long)window_us * 2);
	return 0;
}
