#
# highmon.pl - Highlight monitor for weechat 0.3.0
# Version 1.6
#
# Add 'Highlight Monitor' buffer to log all highlights in one spot
#
# /set plugins.var.perl.highmon.alignment
# The config setting "alignment" can be changed to;
# "channel", "schannel", "channel,nick", "schannel,nick"
# to change how the monitor appears
# The 'schannel' value will only show the buffer number as opposed to
# 'server#channel'
#
# /set plugins.var.perl.highmon.short_names
# Setting this to 'on' will trim the network name from chanmon, ala buffers.pl
#
# /set plugins.var.perl.highmon.color_buf
# This turns colored buffer names on or off, you can also set a single fixed color by using a weechat color name.
# This *must* be a valid color name, or weechat will likely do unexpected things :)
#
# /set plugins.var.perl.highmon.hotlist_show
# Setting this to 'on' will let the highmon buffer appear in hotlists
# (status bar/buffer.pl)
#
# /set plugins.var.perl.highmon.away_only
# Setting this to 'on' will only put messages in the highmon buffer when
# you set your status to away
#
# History:
# 2009-09-07, KenjiE20 <longbow@longbowslair.co.uk>:
#	v1.6:	-feature: colored buffer names
#		-change: version sync with chanmon
# 2009-09-05, KenjiE20 <longbow@longbowslair.co.uk>:
#	v1.2:	-fix: disable buffer highlight
# 2009-09-02, KenjiE20 <longbow@longbowslair.co.uk>:
#	v.1.1.1	-change: Stop unsightly text block on '/help'
# 2009-08-10, KenjiE20 <longbow@longbowslair.co.uk>:
#	v1.1:	In-client help added
# 2009-08-02, KenjiE20 <longbow@longbowslair.co.uk>:
#	v1.0:	Initial Public Release
#
# Copyright (c) 2009 by KenjiE20 <longbow@longbowslair.co.uk>
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

my $highmon_buffer = "";

# Replicate info earlier for in-client help
$highmonhelp = weechat::color("bold")."/set plugins.var.perl.highmon.alignment".weechat::color("-bold")."
The config setting \"alignment\" can be changed to;
\"channel\", \"schannel\", \"channel,nick\", \"schannel,nick\"
to change how the monitor appears
The 'schannel' value will only show the buffer number as opposed to 'server#channel'

".weechat::color("bold")."/set plugins.var.perl.highmon.short_names".weechat::color("-bold")."
Setting this to 'on' will trim the network name from chanmon, ala buffers.pl

".weechat::color("bold")."/set plugins.var.perl.highmon.color_buf".weechat::color("-bold")."
This turns colored buffer names on or off, you can also set a single fixed color by using a weechat color name.
This ".weechat::color("bold")."must".weechat::color("-bold")." be a valid color name, or weechat will likely do unexpected things :)

".weechat::color("bold")."/set plugins.var.perl.highmon.hotlist_show".weechat::color("-bold")."
Setting this to 'on' will let the highmon buffer appear in hotlists (status bar/buffer.pl)

".weechat::color("bold")."/set plugins.var.perl.highmon.away_only".weechat::color("-bold")."
Setting this to 'on' will only put messages in the highmon buffer when you set your status to away";

sub highmon_new_message
{
	# Change if config changes
	if (weechat::config_get_plugin("hotlist_show" eq "off"))
	{
		weechat::buffer_set($highmon_buffer, "notify", "0");
	}
	else
	{
		weechat::buffer_set($highmon_buffer, "notify", "3");
	}

	my $net = "";
	my $chan = "";
	my $nick = "";
	my $outstr = "";
	my $curbuf = "";

#	DEBUG point
#	$string = "\t"."0: ".$_[0]." 1: ".$_[1]." 2: ".$_[2]." 3: ".$_[3]." 4: ".$_[4]." 5: ".$_[5]." 6: ".$_[6]." 7: ".$_[7];
#	weechat::print("", "\t".$string);

	$cb_datap = $_[0];
	$cb_bufferp = $_[1];
	$cb_date = $_[2];
	$cb_tags = $_[3];
	$cb_disp = $_[4];
	$cb_high = $_[5];
	$cb_prefix = $_[6];
	$cb_msg = $_[7];

	if ($cb_high == "1")
	{
		$away = weechat::buffer_get_string($cb_bufferp, "localvar_away");
		if (weechat::config_get_plugin("away_only") ne "on" || ($away ne ""))
		{
			$bufname = weechat::buffer_get_string($cb_bufferp, 'name');
			if ($bufname =~ /(.*)\.([#&\+!])(.*)/)
			{
				$bufname = $1.$2.$3;
				if (!($cb_prefix =~ / \*/) && !($cb_prefix =~ /--/))
				{
					$uncolnick = weechat::string_remove_color($cb_prefix, "");
					$nick = " <".weechat::color("chat_highlight").$uncolnick.weechat::color("reset").">";
				}
				else
				{
					$uncolnick = weechat::string_remove_color($cb_prefix, "");
					$nick = weechat::color("chat_highlight").$uncolnick.weechat::color("reset");
				}

				$bufname = format_buffer($cb_bufferp, $bufname);

				if (weechat::config_get_plugin("alignment") eq "channel")
				{
					$nick =~ s/\s(.*)/$1/;
					$outstr = $bufname."\t".$nick." ".$cb_msg;
				}
				elsif (weechat::config_get_plugin("alignment") eq "schannel")
				{
					$nick =~ s/\s(.*)/$1/;
					$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').weechat::color("reset");
					$outstr = $bufname."\t".$nick." ".$cb_msg;
				}
				elsif (weechat::config_get_plugin("alignment") eq "channel,nick")
				{
					$outstr = $bufname.":".$nick."\t".$cb_msg;
				}
				elsif (weechat::config_get_plugin("alignment") eq "schannel,nick")
				{
					$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').weechat::color("reset");
					$outstr = $bufname.":".$nick."\t".$cb_msg;
				}
				else
				{
					$outstr = "\t".$bufname.":".$nick." ".$cb_msg;
				}

				weechat::print($highmon_buffer, $outstr);
			}
		}
	}
	return weechat::WEECHAT_RC_OK;
}

sub format_buffer
{
	$cb_bufferp = $_[0];
	$bufname = $_[1];

	if (weechat::config_get_plugin("short_names") eq "on")
	{
		$bufname = weechat::buffer_get_string($cb_bufferp, 'short_name');
	}

	if (weechat::config_get_plugin("color_buf") eq "on")
	{
		$color = 0;
		@char_array = split(//,weechat::buffer_get_string($cb_bufferp, 'name'));
		foreach $char (@char_array)
		{
			$color += ord($char);
		}
		$color %= 10;
		$color = sprintf "weechat.color.chat_nick_color%02d", $color+1;
		$color = weechat::config_get($color);
		$color = weechat::config_string($color);
		$bufname = weechat::color($color).$bufname.weechat::color("reset");
	}
	elsif (weechat::config_get_plugin("color_buf") ne "off")
	{
		$color = weechat::config_get_plugin("color_buf");
		$bufname = weechat::color($color).$bufname.weechat::color("reset");
	}

	return $bufname;
}

sub highmon_buffer_close
{
	$highmon_buffer = "";
	return weechat::WEECHAT_RC_OK;
}

sub highmon_buffer_setup
{
	return weechat::WEECHAT_RC_OK;
}

sub highmon_buffer_open
{
	$highmon_buffer = weechat::buffer_search("perl", "highmon");

	if ($highmon_buffer eq "")
	{
		$highmon_buffer = weechat::buffer_new("highmon", "highmon_buffer_setup", "", "", "highmon_buffer_close", "");
	}

	if ($highmon_buffer ne "")
	{
		if (weechat::config_get_plugin("hotlist_show" eq "off"))
		{
			weechat::buffer_set($highmon_buffer, "notify", "0");
		}
		weechat::buffer_set($highmon_buffer, "highlight_words", "-");
		weechat::buffer_set($highmon_buffer, "title", "Highlight Monitor");
	}
	return weechat::WEECHAT_RC_OK;
}

sub highmon_buffer_input
{
	return weechat::WEECHAT_RC_OK;
}

sub print_help
{
	weechat::print("", "\t".weechat::color("bold")."Highmon Help".weechat::color("-bold")."\n\n");
	weechat::print("", "\t".$highmonhelp);
	return weechat::WEECHAT_RC_OK;
}

weechat::register("highmon", "KenjiE20", "1.6", "GPL3", "Highlight Monitor", "", "");
weechat::hook_print("", "", "", 0, "highmon_new_message", "");
weechat::hook_command("highmon", "Highmon help", "", $highmonhelp, "", "print_help", "");

weechat::hook_config("plugins.var.perl.highmon.*", "", "");
if (!(weechat::config_is_set_plugin ("alignment")))
{
	weechat::config_set_plugin("alignment", "channel");
}
if (weechat::config_get_plugin("alignment") eq "")
{
	weechat::config_set_plugin("alignment", "none");
}
if (!(weechat::config_is_set_plugin ("short_names")))
{
	weechat::config_set_plugin("short_names", "on");
}
if (!(weechat::config_is_set_plugin ("color_buf")))
{
	weechat::config_set_plugin("short_names", "on");
}
if (!(weechat::config_is_set_plugin ("hotlist_show")))
{
	weechat::config_set_plugin("hotlist_show", "off");
}
if (!(weechat::config_is_set_plugin ("away_only")))
{
	weechat::config_set_plugin("away_only", "off");
}

highmon_buffer_open();