project(
    'wayfire-gnome-shell',
    'cpp',
    version: '0.1.0',
    license: 'MIT',
    meson_version: '>=0.51.0',
    default_options: [
        'cpp_std=c++17',
        'c_std=c11',
        'warning_level=2',
        'werror=false',
    ],
)

wayfire = dependency('wayfire')

add_project_arguments(['-DWLR_USE_UNSTABLE'], language: ['cpp', 'c'])
add_project_arguments(['-DWAYFIRE_PLUGIN'], language: ['cpp', 'c'])

# Use local prefix instead of wayfire's system paths
plugin_dir = join_paths(get_option('libdir'), 'wayfire')
metadata_dir = join_paths(get_option('datadir'), 'wayfire', 'metadata')

subdir('src')
subdir('metadata')
