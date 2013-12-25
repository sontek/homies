#
# highmon.pl - Highlight Monitoring for weechat 0.3.0
# Version 2.4
#
# Add 'Highlight Monitor' buffer/bar to log all highlights in one spot
#
# Usage:
# /highmon [help] | [monitor [channel [server]]] | [clean default|orphan|all]
#  Command wrapper for highmon commands
#
# /highmon clean default|orphan|all will clean the config section of default 'on' entries,
#  channels you are no longer joined, or both
#
# /highmon monitor [channel] [server] is used to toggle a highlight monitoring on and off, this
#  can be used in the channel buffer for the channel you wish to toggle, or be given
#  with arguments e.g. /highmon monitor #weechat freenode
#
# /set plugins.var.perl.highmon.alignment
#  The config setting "alignment" can be changed to;
#  "channel", "schannel", "nchannel", "channel,nick", "schannel,nick", "nchannel,nick"
#  to change how the monitor appears
#  The 'channel'  value will show: "#weechat"
#  The 'schannel' value will show: "6"
#  The 'nchannel' value will show: "6:#weechat"
#
# /set plugins.var.perl.highmon.short_names
#  Setting this to 'on' will trim the network name from highmon, ala buffers.pl
#
# /set plugins.var.perl.highmon.merge_private
#  Setting this to 'on' will merge private messages to highmon's display
#
# /set plugins.var.perl.highmon.color_buf
#  This turns colored buffer names on or off, you can also set a single fixed color by using a weechat color name.
#  This *must* be a valid color name, or weechat will likely do unexpected things :)
#
# /set plugins.var.perl.highmon.hotlist_show
#  Setting this to 'on' will let the highmon buffer appear in hotlists
#  (status bar/buffer.pl)
#
# /set plugins.var.perl.highmon.away_only
#  Setting this to 'on' will only put messages in the highmon buffer when
#  you set your status to away
#
# /set plugins.var.perl.highmon.logging
#  Toggles logging status for highmon buffer (default: off)
#
# /set plugins.var.perl.highmon.output
#  Changes where output method of highmon; takes either "bar" or "buffer" (default; buffer)
# /set plugins.var.perl.highmon.bar_lines
#  Changes the amount of lines the output bar will hold.
#  (Only appears once output has been set to bar, defaults to 10)
# /set plugins.var.perl.highmon.bar_scrolldown
#  Toggles the bar scrolling at the bottom when new highlights are received
#  (Only appears once output has been set to bar, defaults to off)
#
# /set plugins.var.perl.highmon.nick_prefix
# /set plugins.var.perl.highmon.nick_suffix
#  Sets the prefix and suffix chars in the highmon buffer
#  (Defaults to <> if nothing set, and blank if there is)
#
# servername.#channel
#  servername is the internal name for the server (set when you use /server add)
#  #channel is the channel name, (where # is whatever channel type that channel happens to be)
#

# Bugs and feature requests at: https://github.com/KenjiE20/highmon

# History:
# 2013-12-04, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.4:	-add: Support for eval style colour codes in time format used for bar output
# 2013-10-22, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.3.3.2:	-fix: Typo in fix command
# 2013-10-10, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.3.3.1:	-fix: Typo in closed buffer warning
# 2013-10-07, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.3.3:	-add: Warning and fixer for accidental buffer closes
# 2013-01-15, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.3.2:	-fix: Let bar output use the string set in weechat's config option
#			-add: github info
# 2012-04-15, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.3.1:	-fix: Colour tags in bar timestamp string
# 2012-02-28, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.3:	-feature: Added merge_private option to display private messages (default: off)
#			-fix: Channel name colours now show correctly when set to on
# 2011-08-07, Sitaktif <romainchossart_at_gmail.com>:
#	v2.2.1:	-feature: Add "bar_scrolldown" option to have the bar display the latest hl at anytime
#			-fix: Set up bar-specific config at startup if 'output' is already configured as 'bar'
# 2010-12-22, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.2:	-change: Use API instead of config to find channel colours, ready for 0.3.4 and 256 colours
# 2010-12-13, idl0r & KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.1.3:	-fix: perl errors caused by bar line counter
#			-fix: Add command list to inbuilt help
# 2010-09-30, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.1.2:	-fix: logging config was not correctly toggling back on (thanks to sleo for noticing)
#			-version sync w/ chanmon
# 2010-08-27, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.1: -feature: Add 'nchannel' option to alignment to display buffer and name
# 2010-04-25, KenjiE20 <longbow@longbowslair.co.uk>:
#	v2.0:	Release as version 2.0
# 2010-04-24, KenjiE20 <longbow@longbowslair.co.uk>:
#	v1.9:	Rewrite for v2.0
#		Bring feature set in line with chanmon 2.0
#		-code change: Made more subs to shrink the code down in places
#		-fix: Stop highmon attempting to double load/hook
#		-fix: Add version dependant check for away status
# 2010-01-25, KenjiE20 <longbow@longbowslair.co.uk>:
#       v1.7:   -fixture: Let highmon be aware of nick_prefix/suffix
#                       and allow custom prefix/suffix for chanmon buffer
#                       (Defaults to <> if nothing set, and blank if there is)
#               (Thanks to m4v for these)
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

@bar_lines = ();
@bar_lines_time = ();
# Replicate info earlier for in-client help

$highmonhelp = weechat::color("bold")."/highmon [help] | [monitor [channel [server]]] | [clean default|orphan|all]".weechat::color("-bold")."
 Command wrapper for highmon commands

".weechat::color("bold")."/highmon clean default|orphan|all".weechat::color("-bold")." will clean the config section of default 'on' entries, channels you are no longer joined, or both

".weechat::color("bold")."/highmon monitor [channel] [server]".weechat::color("-bold")." is used to toggle a highlight monitoring on and off, this can be used in the channel buffer for the channel you wish to toggle, or be given with arguments e.g. /highmon monitor #weechat freenode

".weechat::color("bold")."/set plugins.var.perl.highmon.alignment".weechat::color("-bold")."
 The config setting \"alignment\" can be changed to;
 \"channel\", \"schannel\", \"nchannel\", \"channel,nick\", \"schannel,nick\", \"nchannel,nick\"
 to change how the monitor appears
 The 'channel'  value will show: \"#weechat\"
 The 'schannel' value will show: \"6\"
 The 'nchannel' value will show: \"6:#weechat\"

".weechat::color("bold")."/set plugins.var.perl.highmon.short_names".weechat::color("-bold")."
 Setting this to 'on' will trim the network name from highmon, ala buffers.pl

".weechat::color("bold")."/set plugins.var.perl.highmon.merge_private".weechat::color("-bold")."
 Setting this to 'on' will merge private messages to highmon's display

".weechat::color("bold")."/set plugins.var.perl.highmon.color_buf".weechat::color("-bold")."
 This turns colored buffer names on or off, you can also set a single fixed color by using a weechat color name.
 This ".weechat::color("bold")."must".weechat::color("-bold")." be a valid color name, or weechat will likely do unexpected things :)

".weechat::color("bold")."/set plugins.var.perl.highmon.hotlist_show".weechat::color("-bold")."
Setting this to 'on' will let the highmon buffer appear in hotlists (status bar/buffer.pl)

".weechat::color("bold")."/set plugins.var.perl.highmon.away_only".weechat::color("-bold")."
Setting this to 'on' will only put messages in the highmon buffer when you set your status to away

".weechat::color("bold")."/set plugins.var.perl.highmon.logging".weechat::color("-bold")."
 Toggles logging status for highmon buffer (default: off)

".weechat::color("bold")."/set plugins.var.perl.highmon.output".weechat::color("-bold")."
 Changes where output method of highmon; takes either \"bar\" or \"buffer\" (default; buffer)
".weechat::color("bold")."/set plugins.var.perl.highmon.bar_lines".weechat::color("-bold")."
 Changes the amount of lines the output bar will hold.
 (Only appears once output has been set to bar, defaults to 10)
".weechat::color("bold")."/set plugins.var.perl.highmon.bar_scrolldown".weechat::color("-bold")."
 Toggles the bar scrolling at the bottom when new highlights are received
 (Only appears once output has been set to bar, defaults to off)

".weechat::color("bold")."/set plugins.var.perl.highmon.nick_prefix".weechat::color("-bold")."
".weechat::color("bold")."/set plugins.var.perl.highmon.nick_suffix".weechat::color("-bold")."
 Sets the prefix and suffix chars in the highmon buffer
 (Defaults to <> if nothing set, and blank if there is)

".weechat::color("bold")."servername.#channel".weechat::color("-bold")."
 servername is the internal name for the server (set when you use /server add)
 #channel is the channel name, (where # is whatever channel type that channel happens to be)";
# Print verbose help
sub print_help
{
	weechat::print("", "\t".weechat::color("bold")."Highmon Help".weechat::color("-bold")."\n\n");
	weechat::print("", "\t".$highmonhelp);
	return weechat::WEECHAT_RC_OK;
}

# Bar item build
sub highmon_bar_build
{
	# Get max lines
	$max_lines = weechat::config_get_plugin("bar_lines");
	$max_lines = $max_lines ? $max_lines : 10;
	$str = '';
	$align_num = 0;
	$count = 0;
	# Keep lines within max
	while ($#bar_lines > $max_lines)
	{
		shift(@bar_lines);
		shift(@bar_lines_time);
	}
	# So long as we have some lines, build a string
	if (@bar_lines)
	{
		# Build loop
		$sep = " ".weechat::config_string(weechat::config_get("weechat.look.prefix_suffix"))." ";
		foreach(@bar_lines)
		{
			# Find max align needed
			$prefix_num = (index(weechat::string_remove_color($_, ""), $sep));
			$align_num = $prefix_num if ($prefix_num > $align_num);
		}
		foreach(@bar_lines)
		{
			# Get align for this line
			$prefix_num = (index(weechat::string_remove_color($_, ""), $sep));

			# Make string
			$str = $str.$bar_lines_time[$count]." ".(" " x ($align_num - $prefix_num)).$_."\n";
			# Increment count for sync with time list
			$count++;
		}
	}
	return $str;
}

# Make a new bar
sub highmon_bar_open
{
	# Make the bar item
	weechat::bar_item_new("highmon", "highmon_bar_build", "");

	$highmon_bar = weechat::bar_new ("highmon", "off", 100, "root", "", "bottom", "vertical", "vertical", 0, 0, "default", "cyan", "default", "on", "highmon");

	return weechat::WEECHAT_RC_OK;
}
# Close bar
sub highmon_bar_close
{
	# Find if bar exists
	$highmon_bar = weechat::bar_search("highmon");
	# If is does, close it
	if ($highmon_bar ne "")
	{
		weechat::bar_remove($highmon_bar);
	}

	# Find if bar item exists
	$highmon_bar_item = weechat::bar_item_search("highmon_bar");
	# If is does, close it
	if ($highmon_bar_item ne "")
	{
		weechat::bar_remove($highmon_bar_item);
	}

	@bar_lines = ();
	return weechat::WEECHAT_RC_OK;
}

# Make a new buffer
sub highmon_buffer_open
{
	# Search for pre-existing buffer
	$highmon_buffer = weechat::buffer_search("perl", "highmon");

	# Make a new buffer
	if ($highmon_buffer eq "")
	{
		$highmon_buffer = weechat::buffer_new("highmon", "highmon_buffer_input", "", "highmon_buffer_close", "");
	}

	# Turn off notify, highlights
	if ($highmon_buffer ne "")
	{
		if (weechat::config_get_plugin("hotlist_show" eq "off"))
		{
			weechat::buffer_set($highmon_buffer, "notify", "0");
		}
		weechat::buffer_set($highmon_buffer, "highlight_words", "-");
		weechat::buffer_set($highmon_buffer, "title", "Highlight Monitor");
		# Set no_log
		if (weechat::config_get_plugin("logging") eq "off")
		{
			weechat::buffer_set($highmon_buffer, "localvar_set_no_log", "1");
		}
	}
	return weechat::WEECHAT_RC_OK;
}
# Buffer input has no action
sub highmon_buffer_input
{
	return weechat::WEECHAT_RC_OK;
}
# Close up
sub highmon_buffer_close
{
	$highmon_buffer = "";
	# If user hasn't changed output style warn user
	if (weechat::config_get_plugin("output") eq "buffer")
	{
		weechat::print("", "\tHighmon buffer has been closed but output is still set to buffer, unusual results may occur. To recreate the buffer use ".weechat::color("bold")."/highmon fix".weechat::color("-bold"));
	}
	return weechat::WEECHAT_RC_OK;
}

# Highmon command wrapper
sub highmon_command_cb
{
	$data = $_[0];
	$buffer = $_[1];
	$args = $_[2];
	my $cmd = '';
	my $arg = '';

	if ($args ne "")
	{
		# Split argument up
		@arg_array = split(/ /,$args);
		# Take first as command
		$cmd = shift(@arg_array);
		# Rebuild string to pass to subs
		if (@arg_array)
		{
			$arg = join(" ", @arg_array);
		}
	}

	# Help command
	if ($cmd eq "" || $cmd eq "help")
	{
		print_help();
	}
	# /monitor command
	elsif ($cmd eq "monitor")
	{
		highmon_toggle($data, $buffer, $arg);
	}
	# /highclean command
	elsif ($cmd eq "clean")
	{
		highmon_config_clean($data, $buffer, $arg);
	}
	# Fix closed buffer
	elsif ($cmd eq "fix")
	{
		if (weechat::config_get_plugin("output") eq "buffer" && $highmon_buffer eq "")
		{
			highmon_buffer_open();
		}
	}
	return weechat::WEECHAT_RC_OK;
}

# Clean up config entries
sub highmon_config_clean
{
	$data = $_[0];
	$buffer = $_[1];
	$args = $_[2];

	# Don't do anything if bad option given
	if ($args ne "default" && $args ne "orphan"  && $args ne "all")
	{
		weechat::print("", "\thighmon.pl: Unknown option");
		return weechat::WEECHAT_RC_OK;
	}

	@chans = ();
	# Load an infolist of highmon options
	$infolist = weechat::infolist_get("option", "", "*highmon*");
	while (weechat::infolist_next($infolist))
	{
		$name = weechat::infolist_string($infolist, "option_name");
		$name =~ s/perl\.highmon\.(\w*)\.([#&\+!])(.*)/$1.$2$3/;
		if ($name =~ /^(.*)\.([#&\+!])(.*)$/)
		{
			$action = 0;
			# Clean up all 'on's
			if ($args eq "default" || $args eq "all")
			{
				# If value in config is "on"
				if (weechat::config_get_plugin($name) eq "on")
				{
					# Unset and if successful flag as changed
					$rc = weechat::config_unset_plugin($name);
					if ($rc eq weechat::WEECHAT_CONFIG_OPTION_UNSET_OK_REMOVED)
					{
						$action = 1;
					}
				}
			}
			# Clean non joined
			if ($args eq "orphan" || $args eq "all")
			{
				# If we can't find the buffer for this entry
				if (weechat::buffer_search("irc", $name) eq "")
				{
					# Unset and if successful flag as changed
					$rc = weechat::config_unset_plugin($name);
					if ($rc eq weechat::WEECHAT_CONFIG_OPTION_UNSET_OK_REMOVED)
					{
						$action = 1;
					}
				}
			}
			# Add changed entry names to list
			push (@chans, $name) if ($action);
		}
	}
	weechat::infolist_free($infolist);
	# If channels were cleaned from config
	if (@chans)
	{
		# If only one entry
		if (@chans == 1)
		{
			$str = "\thighmon.pl: Cleaned ".@chans." entry from the config:";
		}
		else
		{
			$str = "\thighmon.pl: Cleaned ".@chans." entries from the config:";
		}
		# Build a list of channels
		foreach(@chans)
		{
			$str = $str." ".$_;
		}
		# Print what happened
		weechat::print("",$str);
	}
	# Config seemed to be clean
	else
	{
		weechat::print("", "\thighmon.pl: No entries removed");
	}
	return weechat::WEECHAT_RC_OK;
}

# Check config elements
sub highmon_config_init
{
	# Alignment default
	if (!(weechat::config_is_set_plugin ("alignment")))
	{
		weechat::config_set_plugin("alignment", "channel");
	}
	if (weechat::config_get_plugin("alignment") eq "")
	{
		weechat::config_set_plugin("alignment", "none");
	}

	# Short name default
	if (!(weechat::config_is_set_plugin ("short_names")))
	{
		weechat::config_set_plugin("short_names", "off");
	}

	# Coloured names default
	if (!(weechat::config_is_set_plugin ("color_buf")))
	{
		weechat::config_set_plugin("color_buf", "on");
	}

	# Hotlist show default
	if (!(weechat::config_is_set_plugin ("hotlist_show")))
	{
		weechat::config_set_plugin("hotlist_show", "off");
	}

	# Away only default
	if (!(weechat::config_is_set_plugin ("away_only")))
	{
		weechat::config_set_plugin("away_only", "off");
	}

	# highmon log default
	if (!(weechat::config_is_set_plugin ("logging")))
	{
		weechat::config_set_plugin("logging", "off");
	}

	# Output default
	if (!(weechat::config_is_set_plugin ("output")))
	{
		weechat::config_set_plugin("output", "buffer");
	}

	# Private message merging
	if (!(weechat::config_is_set_plugin ("merge_private")))
	{
		weechat::config_set_plugin("merge_private", "off");
	}

	# Set bar config in case output was set to "bar" before even changing the setting
	if (weechat::config_get_plugin("output") eq "bar")
	{
		# Output bar lines default
		if (!(weechat::config_is_set_plugin ("bar_lines")))
		{
			weechat::config_set_plugin("bar_lines", "10");
		}
		if (!(weechat::config_is_set_plugin ("bar_scrolldown")))
		{
			weechat::config_set_plugin("bar_scrolldown", "off");
		}
	}

	# Check for exisiting prefix/suffix chars, and setup accordingly
	$prefix = weechat::config_get("irc.look.nick_prefix");
	$prefix = weechat::config_string($prefix);
	$suffix = weechat::config_get("irc.look.nick_suffix");
	$suffix = weechat::config_string($suffix);

	if (!(weechat::config_is_set_plugin("nick_prefix")))
	{
		if ($prefix eq "" && $suffix eq "")
		{
			weechat::config_set_plugin("nick_prefix", "<");
		}
		else
		{
			weechat::config_set_plugin("nick_prefix", "");
		}
	}

	if (!(weechat::config_is_set_plugin("nick_suffix")))
	{
		if ($prefix eq "" && $suffix eq "")
		{
			weechat::config_set_plugin("nick_suffix", ">");
		}
		else
		{
			weechat::config_set_plugin("nick_suffix", "");
		}
	}
}

# Get config updates
sub highmon_config_cb
{
	$point = $_[0];
	$name = $_[1];
	$value = $_[2];

	$name =~ s/^plugins\.var\.perl\.highmon\.//;

	# Set logging on buffer
	if ($name eq "logging")
	{
		# Search for pre-existing buffer
		$highmon_buffer = weechat::buffer_search("perl", "highmon");
		if ($value eq "off")
		{
			weechat::buffer_set($highmon_buffer, "localvar_set_no_log", "1");
		}
		else
		{
			weechat::buffer_set($highmon_buffer, "localvar_del_no_log", "");
		}
	}
	# Output changer
	elsif ($name eq "output")
	{
		if ($value eq "bar")
		{
			# Search for pre-existing buffer
			$highmon_buffer = weechat::buffer_search("perl", "highmon");
			# Close if it exists
			if ($highmon_buffer ne "")
			{
				weechat::buffer_close($highmon_buffer)
			}

			# Output bar lines default
			if (!(weechat::config_is_set_plugin ("bar_lines")))
			{
				weechat::config_set_plugin("bar_lines", "10");
			}
			if (!(weechat::config_is_set_plugin ("bar_scrolldown")))
			{
				weechat::config_set_plugin("bar_scrolldown", "off");
			}
			# Make a bar if doesn't exist
			highmon_bar_open();
		}
		elsif ($value eq "buffer")
		{
			# If a bar exists, close it
			highmon_bar_close();
			# Open buffer
			highmon_buffer_open();
		}

	}
	# Change if hotlist config changes
	elsif ($name eq "hotlist_show")
	{
		# Search for pre-existing buffer
		$highmon_buffer = weechat::buffer_search("perl", "highmon");
		if ($value eq "off" && $highmon_buffer)
		{
			weechat::buffer_set($highmon_buffer, "notify", "0");
		}
		elsif ($value ne "off" && $highmon_buffer)
		{
			weechat::buffer_set($highmon_buffer, "notify", "3");
		}
	}
	elsif ($name eq "weechat.look.prefix_suffix")
	{
		if (weechat::config_get_plugin("output") eq "bar")
		{
			@bar_lines = ();
			weechat::print("", "\thighmon: weechat.look.prefix_suffix changed, clearing highmon bar");
			weechat::bar_item_update("highmon");
		}
	}
	return weechat::WEECHAT_RC_OK;
}

# Set up weechat hooks / commands
sub highmon_hook
{
	weechat::hook_print("", "", "", 0, "highmon_new_message", "");
	weechat::hook_command("highclean", "Highmon config clean up", "default|orphan|all", " default: Cleans all config entries with the default \"on\" value\n  orphan: Cleans all config entries for channels you aren't currently joined\n     all: Does both defaults and orphan", "default|orphan|all", "highmon_config_clean", "");

	weechat::hook_command("highmon", "Highmon help", "[help] | [monitor [channel [server]]] | [clean default|orphan|all]", "   help: Print help on config options for highmon\n monitor: Toggles monitoring for a channel\n  clean: Highmon config clean up (/highclean)", "help || monitor %(irc_channels) %(irc_servers) || clean default|orphan|all", "highmon_command_cb", "");

	weechat::hook_config("plugins.var.perl.highmon.*", "highmon_config_cb", "");
	weechat::hook_config("weechat.look.prefix_suffix", "highmon_config_cb", "");
}

# Main body, Callback for hook_print
sub highmon_new_message
{
	my $net = "";
	my $chan = "";
	my $nick = "";
	my $outstr = "";
	my $window_displayed = "";
	my $dyncheck = "0";

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

	# Only work on highlighted messages or private message when enabled
	if ($cb_high == "1" || (weechat::config_get_plugin("merge_private") eq "on" && $cb_tags =~ /notify_private/))
	{
		# Pre bug #29618 (0.3.3) away detect
		if (weechat::info_get("version_number", "") <= 197120)
		{
			$away = '';
			# Get infolist for this server
			$infolist = weechat::infolist_get("irc_server", "", weechat::buffer_get_string($cb_bufferp, "localvar_server"));
			while (weechat::infolist_next($infolist))
			{
				# Get away message is is_away is on
				$away = weechat::infolist_string($infolist, "away_message") if (weechat::infolist_integer($infolist, "is_away"));
			}
			weechat::infolist_free($infolist);
		}
		# Post bug #29618 fix
		else
		{
			$away = weechat::buffer_get_string($cb_bufferp, "localvar_away");
		}
		if (weechat::config_get_plugin("away_only") ne "on" || ($away ne ""))
		{
			# Check buffer name is an IRC channel
			$bufname = weechat::buffer_get_string($cb_bufferp, 'name');
			if ($bufname =~ /(.*)\.([#&\+!])(.*)/)
			{
				# Are we running on this channel
				if (weechat::config_get_plugin($bufname) ne "off" && $cb_disp eq "1")
				{
					# Format nick
					# Line isn't action or topic notify
					if (!($cb_tags =~ /irc_action/) && !($cb_tags =~ /irc_topic/))
					{
						# Strip nick colour
						$uncolnick = weechat::string_remove_color($cb_prefix, "");
						# Format nick
						$nick = " ".weechat::config_get_plugin("nick_prefix").weechat::color("chat_highlight").$uncolnick.weechat::color("reset").weechat::config_get_plugin("nick_suffix");
					}
					# Topic line
					elsif ($cb_tags =~ /irc_topic/)
					{
						$nick = " ".$cb_prefix.weechat::color("reset");
					}
					# Action line
					else
					{
						$uncolnick = weechat::string_remove_color($cb_prefix, "");
						$nick = weechat::color("chat_highlight").$uncolnick.weechat::color("reset");
					}
					# Send to output
					highmon_print ($cb_msg, $cb_bufferp, $nick);
				}
			}
			# Or is private message
			elsif (weechat::config_get_plugin("merge_private") eq "on" && $cb_tags =~ /notify_private/)
			{
				# Strip nick colour
				$uncolnick = weechat::buffer_get_string($cb_bufferp, 'short_name');
				# Format nick
				$nick = " ".weechat::config_get_plugin("nick_prefix").weechat::color("chat_highlight").$uncolnick.weechat::color("reset").weechat::config_get_plugin("nick_suffix");
				#Send to output
				highmon_print ($cb_msg, $cb_bufferp, $nick);
			}
		}
	}
	return weechat::WEECHAT_RC_OK;
}

# Output formatter and printer takes (msg bufpointer nick)
sub highmon_print
{
	$cb_msg = $_[0];
	my $cb_bufferp = $_[1] if ($_[1]);
	my $nick = $_[2] if ($_[2]);

	#Normal channel message
	if ($cb_bufferp && $nick)
	{
		# Format buffer name
		$bufname = format_buffer_name($cb_bufferp);

		# If alignment is #channel | nick msg
		if (weechat::config_get_plugin("alignment") eq "channel")
		{
			$nick =~ s/\s(.*)/$1/;
			# Build string
			$outstr = $bufname."\t".$nick." ".$cb_msg;
		}
		# or if it is channel number | nick msg
		elsif (weechat::config_get_plugin("alignment") eq "schannel")
		{
			$nick =~ s/\s(.*)/$1/;
			# Use channel number instead
			$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').weechat::color("reset");
			# Build string
			$outstr = $bufname."\t".$nick." ".$cb_msg;
		}
		# or if it is number:#channel | nick msg
		elsif (weechat::config_get_plugin("alignment") eq "nchannel")
		{
			$nick =~ s/\s(.*)/$1/;
			# Place channel number in front of formatted name
			$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').":".weechat::color("reset").$bufname;
			# Build string
			$outstr = $bufname."\t".$nick." ".$cb_msg;
		}
		# or if it is #channel nick | msg
		elsif (weechat::config_get_plugin("alignment") eq "channel,nick")
		{
			# Build string
			$outstr = $bufname.":".$nick."\t".$cb_msg;
		}
		# or if it is channel number nick | msg
		elsif (weechat::config_get_plugin("alignment") eq "schannel,nick")
		{
			# Use channel number instead
			$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').weechat::color("reset");
			# Build string
			$outstr = $bufname.":".$nick."\t".$cb_msg;
		}
		# or if it is number:#channel nick | msg
		elsif (weechat::config_get_plugin("alignment") eq "nchannel,nick")
		{
			# Place channel number in front of formatted name
			$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').":".weechat::color("reset").$bufname;
			# Build string
			$outstr = $bufname.":".$nick."\t".$cb_msg;
		}
		# or finally | #channel nick msg
		else
		{
			# Build string
			$outstr = "\t".$bufname.":".$nick." ".$cb_msg;
		}
	}
	# highmon channel toggle message
	elsif ($cb_bufferp && !$nick)
	{
		# Format buffer name
		$bufname = format_buffer_name($cb_bufferp);

		# If alignment is #channel * | *
		if (weechat::config_get_plugin("alignment") =~ /channel/)
		{
			# If it's actually channel number * | *
			if (weechat::config_get_plugin("alignment") =~ /schannel/)
			{
				# Use channel number instead
				$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').weechat::color("reset");
			}
			# Or if it's actually number:#channel * | *
			if (weechat::config_get_plugin("alignment") =~ /nchannel/)
			{
				# Place channel number in front of formatted name
			$bufname = weechat::color("chat_prefix_buffer").weechat::buffer_get_integer($cb_bufferp, 'number').":".weechat::color("reset").$bufname;
			}
			$outstr = $bufname."\t".$cb_msg;
		}
		# or if alignment is | *
		else
		{
			$outstr = $bufname.": ".$cb_msg;
		}
	}
	# highmon dynmon
	elsif (!$cb_bufferp && !$nick)
	{
		$outstr = "\t".$cb_msg;
	}

	# Send string to buffer
	if (weechat::config_get_plugin("output") eq "buffer")
	{
		# Search for and confirm buffer
		$highmon_buffer = weechat::buffer_search("perl", "highmon");
		# Print
		weechat::print($highmon_buffer, $outstr);
	}
	elsif (weechat::config_get_plugin("output") eq "bar")
	{
		# Add time string
		use POSIX qw(strftime);
		$time = strftime(weechat::config_string(weechat::config_get("weechat.look.buffer_time_format")), localtime);
		# Colourise
		if ($time =~ /\$\{(?:color:)?[\w,]+\}/) # Coloured string
		{
			while ($time =~ /\$\{(?:color:)?([\w,]+)\}/)
			{
				$color = weechat::color($1);
				$time =~ s/\$\{(?:color:)?[\w,]+\}/$color/;
			}
			$time .= weechat::color("reset");
		}
		else # Default string
		{
			$colour = weechat::color(weechat::config_string(weechat::config_get("weechat.color.chat_time_delimiters")));
			$reset = weechat::color("reset");
			$time =~ s/(\d*)(.)(\d*)/$1$colour$2$reset$3/g;
		}
		# Push updates to bar lists
		push (@bar_lines_time, $time);

		# Change tab char
		$delim = " ".weechat::color(weechat::config_string(weechat::config_get("weechat.color.chat_delimiters"))).weechat::config_string(weechat::config_get("weechat.look.prefix_suffix")).weechat::color("reset")." ";
		$outstr =~ s/\t/$delim/;

		push (@bar_lines, $outstr);
		# Trigger update
		weechat::bar_item_update("highmon");

		if (weechat::config_get_plugin("bar_scrolldown") eq "on")
		{
			weechat::command("", "/bar scroll highmon * ye")
		}
	}
}

# Start the output display
sub highmon_start
{
	if (weechat::config_get_plugin("output") eq "buffer")
	{
		highmon_buffer_open();
	}
	elsif (weechat::config_get_plugin("output") eq "bar")
	{
		highmon_bar_open();
	}
}

# Takes two optional args (channel server), toggles monitoring on/off
sub highmon_toggle
{
	$data = $_[0];
	$buffer = $_[1];
	$args = $_[2];

	# Check if we've been told what channel to act on
	if ($args ne "")
	{
		# Split argument up
		@arg_array = split(/ /,$args);
		# Check if a server was given
		if ($arg_array[1])
		{
			# Find matching
			$bufp = weechat::buffer_search("irc", $arg_array[1].".".$arg_array[0]);
		}
		else
		{
			$found_chans = 0;
			# Loop through defined servers
			$infolist = weechat::infolist_get("buffer", "", "");
			while (weechat::infolist_next($infolist))
			{
				# Only interesting in IRC buffers
				if (weechat::infolist_string($infolist, "plugin_name") eq "irc")
				{
					# Find buffers that maych
					$sname = weechat::infolist_string($infolist, "short_name");
					if ($sname eq $arg_array[0])
					{
						$found_chans++;
						$bufp = weechat::infolist_pointer($infolist, "pointer");
					}
				}
			}
			weechat::infolist_free($infolist);
			# If the infolist found more than one channel, halt as we need to know which one
			if ($found_chans > 1)
			{
				weechat::print("", "Channel name is not unique, please define server");
				return weechat::WEECHAT_RC_OK;
			}
		}
		# Something didn't return right
		if ($bufp eq "")
		{
			weechat::print("", "Could not find buffer");
			return weechat::WEECHAT_RC_OK;
		}
	}
	else
	{
		# Get pointer from where we are
		$bufp = weechat::current_buffer();
	}
	# Get buffer name
	$bufname = weechat::buffer_get_string($bufp, 'name');
	# Test if buffer is an IRC channel
	if ($bufname =~ /(.*)\.([#&\+!])(.*)/)
	{
		if (weechat::config_get_plugin($bufname) eq "off")
		{
			# If currently off, set on
			weechat::config_set_plugin($bufname, "on");

			# Send to output formatter
			highmon_print("Highlight Monitoring Enabled", $bufp);
			return weechat::WEECHAT_RC_OK;
		}
		elsif (weechat::config_get_plugin($bufname) eq "on" || weechat::config_get_plugin($bufname) eq "")
		{
			# If currently on, set off
			weechat::config_set_plugin($bufname, "off");

			# Send to output formatter
			highmon_print("Highlight Monitoring Disabled", $bufp);
			return weechat::WEECHAT_RC_OK;
		}
	}
}

# Takes a buffer pointer and returns a formatted name
sub format_buffer_name
{
	$cb_bufferp = $_[0];
	$bufname = weechat::buffer_get_string($cb_bufferp, 'name');

	# Set colour from buffer name
	if (weechat::config_get_plugin("color_buf") eq "on")
	{
		# Determine what colour to use
		$color = weechat::info_get("irc_nick_color", $bufname);
		if (!$color)
		{
			$color = 0;
			@char_array = split(//,$bufname);
			foreach $char (@char_array)
			{
				$color += ord($char);
			}
			$color %= 10;
			$color = sprintf "weechat.color.chat_nick_color%02d", $color+1;
			$color = weechat::config_get($color);
			$color = weechat::config_string($color);
			$color = weechat::color($color);
		}

		# Private message just show network
		if (weechat::config_get_plugin("merge_private") eq "on" && weechat::buffer_get_string($cb_bufferp, "localvar_type") eq "private")
		{
			$bufname = weechat::buffer_get_string($cb_bufferp, "localvar_server");
		}
		# Format name to short or 'nicename'
		elsif (weechat::config_get_plugin("short_names") eq "on")
		{
			$bufname = weechat::buffer_get_string($cb_bufferp, 'short_name');
		}
		else
		{
			$bufname =~ s/(.*)\.([#&\+!])(.*)/$1$2$3/;
		}

		# Build a coloured string
		$bufname = $color.$bufname.weechat::color("reset");
	}
	# User set colour name
	elsif (weechat::config_get_plugin("color_buf") ne "off")
	{
		# Private message just show network
		if (weechat::config_get_plugin("merge_private") eq "on" && weechat::buffer_get_string($cb_bufferp, "localvar_type") eq "private")
		{
			$bufname = weechat::buffer_get_string($cb_bufferp, "localvar_server");
		}
		# Format name to short or 'nicename'
		elsif (weechat::config_get_plugin("short_names") eq "on")
		{
			$bufname = weechat::buffer_get_string($cb_bufferp, 'short_name');
		}
		else
		{
			$bufname =~ s/(.*)\.([#&\+!])(.*)/$1$2$3/;
		}

		$color = weechat::config_get_plugin("color_buf");
		$bufname = weechat::color($color).$bufname.weechat::color("reset");
	}
	# Stick with default colour
	else
	{
		# Private message just show network
		if (weechat::config_get_plugin("merge_private") eq "on" && weechat::buffer_get_string($cb_bufferp, "localvar_type") eq "private")
		{
			$bufname = weechat::buffer_get_string($cb_bufferp, "localvar_server");
		}
		# Format name to short or 'nicename'
		elsif (weechat::config_get_plugin("short_names") eq "on")
		{
			$bufname = weechat::buffer_get_string($cb_bufferp, 'short_name');
		}
		else
		{
			$bufname =~ s/(.*)\.([#&\+!])(.*)/$1$2$3/;
		}
	}

	return $bufname;
}

# Check result of register, and attempt to behave in a sane manner
if (!weechat::register("highmon", "KenjiE20", "2.4", "GPL3", "Highlight Monitor", "", ""))
{
	# Double load
	weechat::print ("", "\tHighmon is already loaded");
	return weechat::WEECHAT_RC_OK;
}
else
{
	# Start everything
	highmon_hook();
	highmon_config_init();
	highmon_start();
}
