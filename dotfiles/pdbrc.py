try:
    import termios, sys
    term_fd = sys.stdin.fileno()
    term_echo = termios.tcgetattr(term_fd)
    term_echo[3] = term_echo[3] | termios.ECHO
    term_result = termios.tcsetattr(term_fd, termios.TCSADRAIN, term_echo)
except:
    pass
