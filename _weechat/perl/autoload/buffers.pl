#
# Copyright (c) 2008-2009 by FlashCode <flashcode@flashtux.org>
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

#
# Display sidebar with list of buffers.
#
# History:
# 2009-09-30, FlashCode <flashcode@flashtux.org>:
#     v1.4: remove spaces for indenting when bar position is top/bottom
# 2009-06-14, FlashCode <flashcode@flashtux.org>:
#     v1.3: add option "hide_merged_buffers"
# 2009-06-14, FlashCode <flashcode@flashtux.org>:
#     v1.2: improve display with merged buffers
# 2009-05-02, FlashCode <flashcode@flashtux.org>:
#     v1.1: sync with last API changes
# 2009-02-21, FlashCode <flashcode@flashtux.org>:
#     v1.0: remove timer used to update bar item first time (not needed any more)
# 2009-02-17, FlashCode <flashcode@flashtux.org>:
#     v0.9: fix bug with indenting of private buffers
# 2009-01-04, FlashCode <flashcode@flashtux.org>:
#     v0.8: update syntax for command /set (comments)
# 2008-10-20, Jiri Golembiovsky <golemj@gmail.com>:
#     v0.7: add indenting option
# 2008-10-01, FlashCode <flashcode@flashtux.org>:
#     v0.6: add default color for buffers, and color for current active buffer
# 2008-09-18, FlashCode <flashcode@flashtux.org>:
#     v0.5: fix color for "low" level entry in hotlist
# 2008-09-18, FlashCode <flashcode@flashtux.org>:
#     v0.4: rename option "show_category" to "short_names",
#           remove option "color_slash"
# 2008-09-15, FlashCode <flashcode@flashtux.org>:
#     v0.3: fix bug with priority in hotlist (var not defined)
# 2008-09-02, FlashCode <flashcode@flashtux.org>:
#     v0.2: add color for buffers with activity and config options for
#           colors, add config option to display/hide categories
# 2008-03-15, FlashCode <flashcode@flashtux.org>:
#     v0.1: script creation
#
# Help about settings:
#   display short names (remove text before first "." in buffer name):
#      /set plugins.var.perl.buffers.short_names on
#   use indenting for some buffers like IRC channels:
#      /set plugins.var.perl.buffers.indenting on
#   change colors:
#      /set plugins.var.perl.buffers.color_number color
#      /set plugins.var.perl.buffers.color_default color
#      /set plugins.var.perl.buffers.color_hotlist_low color
#      /set plugins.var.perl.buffers.color_hotlist_message color
#      /set plugins.var.perl.buffers.color_hotlist_private color
#      /set plugins.var.perl.buffers.color_hotlist_highlight color
#      /set plugins.var.perl.buffers.color_current color
#   (replace "color" by your color, which may be "fg" or "fg,bg")
#

use strict;

my $version = "1.4";

# -------------------------------[ config ]-------------------------------------

my $default_short_names         = "off";
my $default_indenting           = "off";
my $default_hide_merged_buffers = "off";

my %hotlist_level = (0 => "low", 1 => "message", 2 => "private", 3 => "highlight");
my %default_color_hotlist = ("low"       => "white",
                             "message"   => "yellow",
                             "private"   => "lightgreen",
                             "highlight" => "magenta");
my $default_color_number = "lightgreen";

# --------------------------------[ init ]--------------------------------------

weechat::register("buffers", "FlashCode <flashcode\@flashtux.org>", $version,
                  "GPL3", "Sidebar with list of buffers", "", "");
if (weechat::config_get_plugin("short_names") eq "")
{
    weechat::config_set_plugin("short_names", $default_short_names);
}
if (weechat::config_get_plugin("indenting") eq "")
{
    weechat::config_set_plugin("indenting", $default_indenting);
}
if (weechat::config_get_plugin("hide_merged_buffers") eq "")
{
    weechat::config_set_plugin("hide_merged_buffers", $default_hide_merged_buffers);
}
if (weechat::config_get_plugin("color_number") eq "")
{
    weechat::config_set_plugin("color_number", $default_color_number);
}
if (weechat::config_get_plugin("color_default") eq "")
{
    weechat::config_set_plugin("color_default", "default");
}
foreach my $level (values %hotlist_level)
{
    if (weechat::config_get_plugin("color_hotlist_".$level) eq "")
    {
        weechat::config_set_plugin("color_hotlist_".$level,
                                   $default_color_hotlist{$level});
    }
}
if (weechat::config_get_plugin("color_current") eq "")
{
    weechat::config_set_plugin("color_current", "lightcyan,red");
}
weechat::bar_item_new("buffers", "build_buffers", "");
weechat::bar_new("buffers", "0", "0", "root", "", "left", "horizontal",
                 "vertical", "0", "0", "default", "default", "default", "1",
                 "buffers");
weechat::hook_signal("buffer_*", "buffers_signal_buffer", "");
weechat::hook_signal("hotlist_*", "buffers_signal_hotlist", "");
weechat::hook_config("plugins.var.perl.buffers.*", "buffers_signal_config", "");
weechat::bar_item_update("buffers");

# ------------------------------------------------------------------------------

sub build_buffers
{
    my $str = "";
    
    # get bar position (left/right/top/bottom)
    my $position = "left";
    my $option = weechat::config_get("weechat.bar.buffers.position");
    if ($option ne "")
    {
        $position = weechat::config_string($option);
    }
    
    # read hotlist
    my %hotlist;
    my $infolist = weechat::infolist_get("hotlist", "", "");
    while (weechat::infolist_next($infolist))
    {
        $hotlist{weechat::infolist_pointer($infolist, "buffer_pointer")} =
            weechat::infolist_integer($infolist, "priority");
    }
    weechat::infolist_free($infolist);
    
    # read buffers list
    my @buffers;
    my @current1 = ();
    my @current2 = ();
    my $old_number = -1;
    my $active_seen = 0;
    $infolist = weechat::infolist_get("buffer", "", "");
    while (weechat::infolist_next($infolist))
    {
        my $buffer = {};
        my $number = weechat::infolist_integer($infolist, "number");
        if ($number ne $old_number)
        {
            @buffers = (@buffers, @current2, @current1);
            @current1 = ();
            @current2 = ();
            $active_seen = 0;
        }
        $old_number = $number;
        my $active = weechat::infolist_integer($infolist, "active");
        if ($active)
        {
            $active_seen = 1;
        }
        $buffer->{"pointer"} = weechat::infolist_pointer($infolist, "pointer");
        $buffer->{"number"} = $number;
        $buffer->{"active"} = $active;
        $buffer->{"current_buffer"} = weechat::infolist_integer($infolist, "current_buffer");
        $buffer->{"short_name"} = weechat::infolist_string($infolist, "short_name");
        $buffer->{"name"} = weechat::infolist_string($infolist, "name");
        if ($active_seen)
        {
            push(@current2, $buffer);
        }
        else
        {
            push(@current1, $buffer);
        }
    }
    @buffers = (@buffers, @current2, @current1);
    weechat::infolist_free($infolist);
    
    # build string with buffers
    $old_number = -1;
    my $hide_merged_buffers = weechat::config_get_plugin("hide_merged_buffers");
    for my $buffer (@buffers)
    {
        if (($hide_merged_buffers eq "on") && (! $buffer->{"active"}))
        {
            next;
        }
        my $color = weechat::config_get_plugin("color_default");
        $color = "default" if ($color eq "");
        my $bg = "";
        if (exists $hotlist{$buffer->{"pointer"}})
        {
            $color = weechat::config_get_plugin("color_hotlist_"
                                                .$hotlist_level{$hotlist{$buffer->{"pointer"}}});
        }
        if ($buffer->{"current_buffer"})
        {
            $color = weechat::config_get_plugin("color_current");
            $bg = $1 if ($color =~ /.*,(.*)/);
        }
        my $color_bg = "";
        $color_bg = weechat::color(",".$bg) if ($bg ne "");
        if ($old_number ne $buffer->{"number"})
        {
            $str .= weechat::color(weechat::config_get_plugin("color_number"))
                .$color_bg
                .$buffer->{"number"}
                .weechat::color("default")
                .$color_bg
                ."."
                .weechat::color($color);
        }
        else
        {
            my $indent = "";
            $indent = ((" " x length($buffer->{"number"}))." ") if (($position eq "left") || ($position eq "right"));
            $str .= weechat::color("default")
                .$color_bg
                .$indent
                .weechat::color($color);
        }
        if ((weechat::config_get_plugin("indenting") eq "on")
            && (($position eq "left") || ($position eq "right")))
        {
            my $type = weechat::buffer_get_string($buffer->{"pointer"}, "localvar_type");
            if (($type eq "channel") || ($type eq "private"))
            {
                $str .= "  ";
            }
        }
        if (weechat::config_get_plugin("short_names") eq "on")
        {
            $str .= $buffer->{"short_name"};
        }
        else
        {
            $str .= $buffer->{"name"};
        }
        $str .= "\n";
        $old_number = $buffer->{"number"};
    }
    
    
    return $str;
}

sub buffers_signal_buffer
{
    weechat::bar_item_update("buffers");
    return weechat::WEECHAT_RC_OK;
}

sub buffers_signal_hotlist
{
    weechat::bar_item_update("buffers");
    return weechat::WEECHAT_RC_OK;
}

sub buffers_signal_config
{
    weechat::bar_item_update("buffers");
    return weechat::WEECHAT_RC_OK;
}
