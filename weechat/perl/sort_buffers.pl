#
# Copyright (c) 2011-2013 by Nils Görs <weechatter@arcor.de>
#
# irc-buffers will be sorted alphabetically or in reverse order
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# 2011-04-14, nils_2:
#     version 0.1: initial release
# 2013-07-16, prurigro:
#     version 0.2: fixed sorting duplicate channel names on different servers
# 2013-07-20, nils_2:
#     version 0.3: add options for script on hook_signal("buffer_opened")
#                : fix bug when irc.look.server_buffer was independent
# 2013-11-24, prurigro:
#     version 0.4: when sorting by server, duplicate channel names don't
#                : get placed with the wrong server 50% of the time

use strict;

my $PRGNAME     = "sort_buffers";
my $VERSION     = "0.4";
my $DESCR       = "irc-buffers will be sorted alphabetically or in reverse order";

my $weechat_version     = "";
my %buffer_struct       = ();                                                           # to store servername and buffername
my $default_start_after_position = 2;
my $i                   = 0;
my $args_all            = 0;
my $args_reverse        = 0;

# default values
my %options = ("sort_order"       => ["default", "sort of buffers \"default\" or \"reverse\" order. This option takes only effect when option \"hook_signal\" is on."],     # or reverse
               "server_wide"      => ["on", "buffers will be sorted server wide. This option takes only effect when option \"hook_signal\" is on."],
               "hook_signal"      => ["off","if \"on\" buffers will be sort every time a buffer was opened. Keep in mind that sorting buffers will delete read_marker."],
    );

# -----------------------------[ config ]-----------------------------------
sub init_config{
  foreach my $option (keys %options){
      if (!weechat::config_is_set_plugin($option)){
          weechat::config_set_plugin($option, $options{$option}[0]);
      }else{
         $options{$option}[0] = weechat::config_get_plugin($option);
      }
      if ($weechat_version >= 0x00030500){
          weechat::config_set_desc_plugin($option, $options{$option}[1] . " (default: " . $options{$option}[0] . ")");
      }
  }
}

sub toggle_config_by_set{
    my ($pointer, $name, $value) = @_;
    $name = substr($name, length("plugins.var.perl.$PRGNAME."), length($name));
    $options{$name}[0] = $value;
    return weechat::WEECHAT_RC_OK;
}

sub get_buffer_list{
  my $infolist = weechat::infolist_get("buffer","","");
  while ( weechat::infolist_next($infolist) ){
    my $buffer_plugin_name = weechat::infolist_string($infolist,"plugin_name");          # get plugin_name of buffer
    my $buffer_name = weechat::infolist_string($infolist,"name");                        # get name of buffer
    my $buffer_full_name = weechat::infolist_string($infolist,"full_name");              # get full_name of buffer
    my $buffer_number = weechat::infolist_integer($infolist,"number");                   # get number of buffer
    my $buffer_short_name = weechat::infolist_string($infolist,"short_name");            # get short_name of buffer
    my $server_name = weechat::buffer_get_string(weechat::infolist_pointer($infolist,"pointer"),"localvar_server");
    my $localvar_type = weechat::buffer_get_string(weechat::infolist_pointer($infolist,"pointer"),"localvar_type");

    if ($buffer_plugin_name eq "core"){
          if ($args_all == 1){
              $buffer_struct{"all_in_one"}{$buffer_full_name}{buffer_short_name} = $server_name;
          }else{
            $buffer_struct{$server_name}{$buffer_name}{buffer_short_name}=$buffer_short_name;
            $buffer_struct{$server_name}{$buffer_name}{$buffer_short_name}{buffer_number}=$buffer_number;
          }
    }
    if ($buffer_plugin_name eq "irc"){                                                                    # irc buffer
      if ($localvar_type eq "server"){                                                                   # server buffer
          if ($args_all == 1){
              $buffer_struct{"all_in_one"}{$buffer_full_name}{buffer_short_name} = $server_name;
          }else{
              $buffer_struct{$server_name}{$buffer_name}{buffer_short_name}=$buffer_short_name;
              $buffer_struct{$server_name}{$buffer_name}{$buffer_short_name}{buffer_number}=$buffer_number;
          }
      }elsif ($localvar_type eq "channel" or $localvar_type eq "private"){
          if ($args_all == 1){
            $buffer_struct{"all_in_one"}{$buffer_name}{buffer_short_name}=$buffer_short_name;
          }else{
              $buffer_struct{$server_name}{$buffer_name}{buffer_short_name}=$buffer_short_name;
              $buffer_struct{$server_name}{$buffer_name}{$buffer_short_name}{buffer_number}=$buffer_number;
          }
      }
    }elsif ($buffer_plugin_name ne "irc" or $buffer_plugin_name ne "core"){                                             # buffer with free content
        if ($args_all == 1){
            $buffer_struct{"all_in_one"}{$buffer_full_name}{buffer_short_name} = $server_name;
        }else{
            $buffer_struct{$server_name}{$buffer_name}{buffer_short_name}=$buffer_short_name;
            $buffer_struct{$server_name}{$buffer_name}{$buffer_short_name}{buffer_number}=$buffer_number;
        }
    }
}
weechat::infolist_free($infolist);
}

# sort z-a
sub reverse_sort{
    if ( $args_all == 1 ){
      foreach my $s ( reverse sort keys %buffer_struct ) {
        foreach my $n ( sort { $buffer_struct{$s}{$b}->{buffer_short_name} cmp $buffer_struct{$s}{$a}->{buffer_short_name}} keys %{$buffer_struct{$s}} ) {
          buffer_movement($n);
        }
      }
    }else{
      foreach my $s ( reverse sort keys %buffer_struct ) {
        foreach my $n ( sort { $buffer_struct{$s}{$b}->{buffer_short_name} cmp $buffer_struct{$s}{$a}->{buffer_short_name}} keys %{$buffer_struct{$s}} ) {
          buffer_movement($n);
        }
      }

      foreach my $s ( reverse sort keys %buffer_struct ) {
        foreach my $n ( reverse sort keys %{$buffer_struct{$s}} ) {
          buffer_movement($n);
        }
      }
    }
}

# sort a-z
sub normal_sort{
    if ( $args_all == 1 ){
      foreach my $s ( sort keys %buffer_struct ) {
        foreach my $n ( sort { $buffer_struct{$s}{$a}->{buffer_short_name} cmp $buffer_struct{$s}{$b}->{buffer_short_name}} keys %{$buffer_struct{$s}} ) {
          buffer_movement($n);
        }
      }
    }else{
      foreach my $s ( sort keys %buffer_struct ) {
        foreach my $n ( sort { $buffer_struct{$s}{$a}->{buffer_short_name} cmp $buffer_struct{$s}{$b}->{buffer_short_name}} keys %{$buffer_struct{$s}} ) {
          buffer_movement($n);
        }
      }

      foreach my $s ( sort keys %buffer_struct ) {
        foreach my $n ( sort keys %{$buffer_struct{$s}} ) {
          buffer_movement($n);
        }
      }
    }
}
sub buffer_movement{
    my $n = $_[0];
    return if ($n eq "");
    weechat::command("","/buffer " . $n);
    weechat::command("","/buffer move " . $i);
    $i++;
}
sub sort_buffers_cmd{
    my ($data, $buffer, $args) = ($_[0], $_[1], $_[2]);

my $from_buffer_called = weechat::buffer_get_string($buffer,"name");            # save buffer, command was executed from

# set variables to default value
    %buffer_struct      = ();
    $args_reverse       = 0;
    $args_all           = 0;
    $i                  = $default_start_after_position;

    if ( $args eq "" ){                                                         # no args! use standard settings
      get_buffer_list();
      normal_sort();
    }else{                                                                      # arguments given
      my @args_array=split(/ /,$args);
      $i = $args_array[0] if ( $args_array[0]  =~ /^\d*$/ );

      if (grep(m/reverse/, @args_array)){
        $args_reverse = 1;
      }
      if (grep(m/all/, @args_array)){
        $args_all = 1;
      }
        get_buffer_list();
        if ( $args_reverse == 0 ){
          normal_sort();
        }else{
          reverse_sort();
        }
    }
    look_server_buffer();
    weechat::command("","/buffer " . $from_buffer_called);                      # switch to buffer, command was executed from
    return weechat::WEECHAT_RC_OK;
}

sub look_server_buffer{
    my $look_server = weechat::config_string(weechat::config_get("irc.look.server_buffer"));
    if ($look_server eq "merge_with_core")
    {
        weechat::command("","/buffer core.weechat");
        weechat::command("","/buffer move 1");
    }
    else
    {
        weechat::command("","/buffer core.weechat");
        weechat::command("","/buffer move 1");
        my $infolist = weechat::infolist_get("buffer","","*server.*");
        weechat::infolist_next($infolist);
        weechat::command("","/buffer " . weechat::infolist_string($infolist, "name"));
        weechat::command("","/buffer move 2");
        weechat::infolist_free($infolist);                                                     # do not forget to free infolist!
    }
}
sub buffer_opened_cb{
    my ($data, $signal, $signal_data) = ($_[0], $_[1], $_[2]);
    my $from_buffer_called = weechat::buffer_get_string($signal_data,"name");    # save buffer command was executed from
    return weechat::WEECHAT_RC_OK if ( $options{hook_signal}[0] eq "off" );

# set variables to default value
    %buffer_struct      = ();
    $args_reverse       = 0;
    $args_all           = 0;
    $i                  = $default_start_after_position;

    if ( $options{server_wide}[0] eq "on" ){
          $args_all = 1;
    }else{
          get_buffer_list();
          normal_sort();
          look_server_buffer();
          weechat::command("","/buffer " . $from_buffer_called);                # go back to buffer command was executed from
          return weechat::WEECHAT_RC_OK;
    }
    if ( $options{sort_order}[0] eq "default" ){                                # or reverse?
          get_buffer_list();
          normal_sort();
    }elsif( $options{sort_order}[0] eq "reverse" ){
          $args_reverse = 1;
          get_buffer_list();
          reverse_sort();
    }
    look_server_buffer();
    weechat::command("","/buffer " . $from_buffer_called);                      # switch to buffer, command was executed from
    return weechat::WEECHAT_RC_OK;
}
# -------------------------------[ init ]-------------------------------------
# first function called by a WeeChat-script.
weechat::register($PRGNAME, "Nils Görs <weechatter\@arcor.de>", $VERSION,
                  "GPL3", $DESCR, "", "");
weechat::hook_command($PRGNAME, $DESCR, "reverse || all",
                      "all       : sort irc-buffers server-wide\n".
                      "reverse   : sort irc-buffers in reverse order\n".
                      "\n".
                      "Examples:\n".
                      "  Sort irc-buffers alphabetically, server by server\n".
                      "    /$PRGNAME\n".
                      "  Sort irc-buffers alphabetically, server-wide\n".
                      "    /$PRGNAME all\n",
                      "all|reverse|%*", "sort_buffers_cmd", "");

$weechat_version = weechat::info_get("version_number", "");

weechat::hook_config("plugins.var.perl.$PRGNAME.*", "toggle_config_by_set", "");
weechat::hook_signal("buffer_opened","buffer_opened_cb","");
init_config();
