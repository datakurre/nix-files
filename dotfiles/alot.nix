{ prefix, ... }:

''
theme = solarized_dark
auto_replyto_mailinglist = True

[accounts]
[[jyu]]
realname = Asko Soukka
address = asko.soukka@jyu.fi
sendmail_command = msmtp --account=jyu -t
sent_box = maildir://${prefix}/.mail/jyu/Sent
draft_box = maildir://${prefix}/.mail/jyu/Drafts
signature = ~/.config/alot/signature-jyu

[[iki]]
realname = Asko Soukka
address = asko.soukka@iki.fi
sendmail_command = msmtp --account=iki -t
sent_box = maildir://${prefix}/.mail/iki/Sent
draft_box = maildir://${prefix}/.mail/iki/Drafts

[bindings]
up = move up
down = move down
page up = move page up
page down = move page down
j = move down
k = move up
'g g' = move first
G = move last
' ' = move page down
'ctrl d' = move halfpage down
'ctrl u' = move halfpage up
@ = refresh
? = help bindings
I = search tag:inbox AND NOT tag:killed
'#' = taglist
shift tab = bprevious
U = search tag:unread
tab = bnext
\ = prompt 'search '
d = bclose
$ = flush
m = compose
o = prompt 'search '
q = exit
';' = bufferlist
':' = prompt
. = repeat

[[bufferlist]]
x = close
enter = open

[[search]]
enter = select
A = toggletags archived
a = toggletags inbox
t = toggletags RT
p = toggletags payments
& = retag killed
! = toggletags flagged
s = toggletags unread
l = retagprompt
O = refineprompt
| = refineprompt

[[envelope]]
a = prompt 'attach ~/'
y = send
P = save
s = 'refine Subject'
f = prompt 'set From '
t = 'refine To'
b = 'refine Bcc'
c = 'refine Cc'
S = togglesign
enter = edit
'g f' = togglesource

[[taglist]]
enter = select

[[thread]]
enter = select
C = fold *
E = unfold *
c = fold
e = unfold
< = fold
> = unfold
'g f' = togglesource
H = toggleheaders
P = print --all --separately --add_tags
S = save --all
g = reply --all
f = forward
p = print --add_tags
n = editnew
b= bounce
s = save
r = reply
| = prompt 'pipeto '
'g j' = move next sibling
'g k' = move previous sibling
'g h' = move parent
'g l' = move first reply
' ' = move next
''
