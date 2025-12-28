# Wayfire GNOME Shell Plugin

A complete GNOME Shell-like desktop experience for the Wayfire compositor.

## Features

- **Top Panel** - Dark panel at the top with:
  - Activities button (left)
  - Clock (center)

- **Activities Overview** - Click Activities or press Super to:
  - See all windows scale and animate to a grid layout
  - Windows smoothly transition using Wayfire's view transformers
  - Hover effect highlights the selected window
  - Click a window to focus it

- **Smooth Animations** - Proper fluid transitions using Wayfire's animation system

## How It Works

This plugin uses Wayfire's **view transformer system** to properly animate windows:

1. When entering overview, a 2D transformer is attached to each view
2. The transformer scales and translates views smoothly based on animation progress
3. A dark overlay fades in behind the transformed views
4. When exiting, views animate back to their original positions
5. Transformers are removed when animation completes

## Installation

### Prerequisites

```bash
# Ubuntu/Debian
sudo apt install \
    meson ninja-build \
    wayfire-dev libwlroots-dev libwf-config-dev \
    libcairo2-dev libpango1.0-dev \
    libgles2-mesa-dev \
    pkg-config

# Fedora
sudo dnf install \
    meson ninja-build \
    wayfire-devel wlroots-devel wf-config-devel \
    cairo-devel pango-devel \
    mesa-libGLES-devel \
    pkg-config

# Arch Linux
sudo pacman -S \
    meson ninja \
    wayfire wlroots \
    cairo pango \
    mesa
```

### Build & Install

```bash
cd wayfire-gnome-shell

meson setup build --prefix=/usr
ninja -C build
sudo ninja -C build install
```

## Configuration

Add to your `~/.config/wayfire.ini`:

```ini
[core]
plugins = gnome-shell ... (your other plugins)

[gnome-shell]
# Toggle overview with Super key
toggle = <super>

# Panel settings
panel_height = 32
panel_color = #1a1a1aE6

# Overview settings  
corner_radius = 12
animation_duration = 300
overview_scale = 0.85
spacing = 20
```

## Usage

1. **Enter Overview**: 
   - Click "Activities" in the top-left corner
   - Or press the `Super` key

2. **In Overview**:
   - Move mouse to hover over windows (they highlight)
   - Click a window to focus it and exit overview
   - Click empty space to exit overview

3. **Exit Overview**:
   - Click any window
   - Click empty space
   - Press `Super` again

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `toggle` | activator | `<super>` | Key/button to toggle overview |
| `panel_height` | int | 32 | Height of top panel (24-64 px) |
| `panel_color` | string | #1a1a1aE6 | Panel background color (hex RRGGBBAA) |
| `corner_radius` | int | 12 | Rounded corner radius (0-30 px) |
| `animation_duration` | int | 300 | Animation time (100-1000 ms) |
| `overview_scale` | double | 0.85 | Window scale in overview (0.5-1.0) |
| `spacing` | int | 20 | Space between windows (5-50 px) |

## Troubleshooting

### Plugin doesn't load

Check Wayfire logs:
```bash
journalctl --user -f | grep -i gnome-shell
```

### Windows don't animate

Make sure you have a recent version of Wayfire with the `view_2d_transformer_t` class.

### Panel not visible

Check that no other panel (wf-panel) is conflicting. You may want to disable wf-shell panel.

## License

MIT License - see LICENSE file

## Credits

- Wayfire compositor: https://github.com/WayfireWM/wayfire
- Inspired by GNOME Shell: https://gitlab.gnome.org/GNOME/gnome-shell
