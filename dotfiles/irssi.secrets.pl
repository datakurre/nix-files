# https://github.com/swick/irssi-passwd
use strict;

use IPC::Open3;
use Irssi;
use Symbol;


my $secrets;
my $help_secrets = '
SECRETS <secrets_id> <command>

  Replaces the string \'<secret>\' with the secret from secrets_id in command and executes the result.
  The secrets_id\'s are defined in \'~/.irssi/secrets\' (or in the file specified by the setting config_file in the section secrets).
  The format of the secret file is \'secrets_id:command\\n\' where command writes the secret to stdout
';

Irssi::settings_add_str('secrets', 'config_file', "$ENV{HOME}/.irssi/secrets");
secrets_init();

sub secrets_init {
  my $cfg_file = Irssi::settings_get_str('config_file');
  open my $handle, $cfg_file or return;

  my %cfg;
  while(<$handle>) {
    my $line = $_;
    if($line =~ /^\s*([a-zA-Z0-9_\-]+)\s*:(.*)$/) {
      my $key = $1;
      my $value = $2;
      $value =~ s/^\s*//;
      $cfg{$key} = $value;
      next;
    }
    if($line =~ /^\s*$/) {
      next;
    }
    Irssi::print("syntax error in file $cfg_file:$.", MSGLEVEL_CLIENTCRAP);
  }

  close $handle;
  $secrets = \%cfg;
}

sub secrets_get_secret {
  my ($secret_key) = @_;
  my ($stdin, $stdout, $stderr);
  my $cmd = $secrets->{$secret_key};
  if(!$cmd) {
    Irssi::print("No command to get secret $secret_key", MSGLEVEL_CLIENTCRAP);
    return;
  }

  $stderr = gensym();
  my $pid = open3($stdin, $stdout, $stderr, $cmd);
  waitpid($pid, 0);

  if($? != 0) {
    my $error = join('', <$stderr>);
    chomp($error);
    Irssi::print("Couldn't get secret: $error", MSGLEVEL_CLIENTCRAP);
    return;
  }

  my $pw = join('', <$stdout>);
  chomp($pw);
  return $pw;
}

Irssi::signal_add_first('server connecting', sub {
  my ($server, @rest) = @_;
  if($server->{address} =~ /\<secret:([a-zA-Z0-9]+)\>/) {
    my $secret = secrets_get_secret($1);
    $server->{address} =~ s/\<secret:[a-zA-Z0-9]+\>/$secret/;
    Irssi::Server::connection_set_key($server, 'address', $server->{address});
  }
  if($server->{port} =~ /\<secret:([a-zA-Z0-9]+)\>/) {
    my $secret = secrets_get_secret($1);
    $server->{port} =~ s/\<secret:[a-zA-Z0-9]+\>/$secret/;
    Irssi::Server::connection_set_key($server, 'port', $server->{port});
  }
  if($server->{password} =~ /\<secret:([a-zA-Z0-9]+)\>/) {
    my $secret = secrets_get_secret($1);
    $server->{password} =~ s/\<secret:[a-zA-Z0-9]+\>/$secret/;
    Irssi::Server::connection_set_key($server, 'password', $server->{password});
  }
  Irssi::signal_continue($server, @rest);
});

Irssi::command_bind('secrets', sub {
  my ($args) = @_;
  my @argv = split(/ /, $args);
  my $argc = @argv;
  if($argc < 2) {
    Irssi::print('Too few arguments', MSGLEVEL_CLIENTCRAP);
    return;
  }
  my $cmd = join(' ', @argv[1..$argc-1]);
  my $pw = secrets_get_secret(@argv[0]);
  $cmd =~ s/\<secret\>/$pw/;
  Irssi::command($cmd);
});

Irssi::command_bind('help', sub {
  if($_[0] =~ /^\s*secrets\s*$/) {
    Irssi::print($help_secrets, MSGLEVEL_CLIENTCRAP);
    Irssi::signal_stop;
  }
});

Irssi::signal_add_first('complete word', sub {
  my ($strings, $window, $word, $linestart, $want_space) = @_;

  return if $linestart ne '/secrets';

  foreach my $key (keys %$secrets) {
    my $sub = substr($key, 0, length($word));
    if($sub eq $word) {
      push(@$strings, $key);
      $$want_space = 1;
    }
  }

  Irssi::signal_stop;
});

