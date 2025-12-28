/**
 * Wayfire GNOME Shell Plugin
 *  copyright andrew pliatsikas
 * A GNOME Shell-like experience for Wayfire:
 * - Top panel with Activities button, clock
 * - Click Activities (or press Super) to enter overview mode
 * - Windows animate smoothly to a grid layout using view transformers
 * - Click a window to focus it and exit overview
 *
 * Copyright (c) 2025
 * Licensed under MIT
 */

#include <wayfire/plugin.hpp>
#include <wayfire/per-output-plugin.hpp>
#include <wayfire/output.hpp>
#include <wayfire/core.hpp>
#include <wayfire/view.hpp>
#include <wayfire/toplevel-view.hpp>
#include <wayfire/signal-definitions.hpp>
#include <wayfire/option-wrapper.hpp>
#include <wayfire/render-manager.hpp>
#include <wayfire/opengl.hpp>
#include <wayfire/region.hpp>
#include <wayfire/geometry.hpp>
#include <wayfire/workspace-set.hpp>
#include <wayfire/workspace-stream.hpp>
#include <wayfire/nonstd/wlroots-full.hpp>
#include <wayfire/util/duration.hpp>
#include <wayfire/plugins/common/util.hpp>
#include <wayfire/scene-operations.hpp>
#include <wayfire/scene-render.hpp>
#include <wayfire/scene.hpp>
#include <wayfire/view-transform.hpp>
#include <wayfire/img.hpp>

#include <cairo.h>
#include <pango/pangocairo.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include <linux/input-event-codes.h>

#include <memory>
#include <vector>
#include <string>
#include <functional>
#include <cmath>
#include <ctime>
#include <chrono>
#include <algorithm>

#ifndef GL_BGRA_EXT
    #define GL_BGRA_EXT 0x80E1
#endif

namespace wf
{
namespace overview
{
// ============================================================================
// Utility
// ============================================================================

struct color_t
{
    float r, g, b, a;

    static color_t from_hex(const std::string& hex)
    {
        color_t c{0.2f, 0.2f, 0.2f, 0.9f};
        if ((hex.length() >= 7) && (hex[0] == '#'))
        {
            c.r = std::stoi(hex.substr(1, 2), nullptr, 16) / 255.0f;
            c.g = std::stoi(hex.substr(3, 2), nullptr, 16) / 255.0f;
            c.b = std::stoi(hex.substr(5, 2), nullptr, 16) / 255.0f;
            if (hex.length() >= 9)
            {
                c.a = std::stoi(hex.substr(7, 2), nullptr, 16) / 255.0f;
            }
        }

        return c;
    }
};

// ============================================================================
// Bezier Curve
// ============================================================================

class BezierCurve
{
  public:
    BezierCurve() = default;

    BezierCurve(float p1x, float p1y, float p2x, float p2y) :
        m_p1{p1x, p1y}, m_p2{p2x, p2y}
    {}

    float getYForX(float x) const
    {
        if (x <= 0.0f)
        {
            return 0.0f;
        }

        if (x >= 1.0f)
        {
            return 1.0f;
        }

        float t = findTForX(x);
        return computeY(t);
    }

  private:
    struct Point
    {
        float x, y;
    };

    Point m_p1{0.0f, 0.0f};
    Point m_p2{1.0f, 1.0f};

    float computeX(float t) const
    {
        float mt = 1.0f - t;
        return 3.0f * mt * mt * t * m_p1.x +
               3.0f * mt * t * t * m_p2.x +
               t * t * t;
    }

    float computeY(float t) const
    {
        float mt = 1.0f - t;
        return 3.0f * mt * mt * t * m_p1.y +
               3.0f * mt * t * t * m_p2.y +
               t * t * t;
    }

    float findTForX(float x) const
    {
        float t = x;
        for (int i = 0; i < 8; i++)
        {
            float currentX = computeX(t);
            float dx = currentX - x;
            if (std::abs(dx) < 0.0001f)
            {
                break;
            }

            float mt = 1.0f - t;
            float derivative = 3.0f * mt * mt * m_p1.x +
                6.0f * mt * t * (m_p2.x - m_p1.x) +
                3.0f * t * t * (1.0f - m_p2.x);

            if (std::abs(derivative) < 0.0001f)
            {
                break;
            }

            t -= dx / derivative;
            t  = std::clamp(t, 0.0f, 1.0f);
        }

        return t;
    }
};

// ============================================================================
// Animated Variable
// ============================================================================

template<typename T>
class AnimatedVar
{
  public:
    AnimatedVar() = default;
    explicit AnimatedVar(T initial) : m_value(initial), m_start(initial), m_goal(initial)
    {}

    void setConfig(BezierCurve *curve, float durationMs)
    {
        m_curve = curve;
        m_durationMs = durationMs;
    }

    void set(T goal, bool animate = true)
    {
        if (!animate || (m_durationMs <= 0))
        {
            m_value = goal;
            m_goal  = goal;
            m_start = goal;
            m_animating = false;
            return;
        }

        m_start = m_value;
        m_goal  = goal;
        m_startTime = std::chrono::high_resolution_clock::now();
        m_animating = true;
    }

    void warp(T value)
    {
        m_value = value;
        m_goal  = value;
        m_start = value;
        m_animating = false;
    }

    bool tick()
    {
        if (!m_animating)
        {
            return false;
        }

        auto now = std::chrono::high_resolution_clock::now();
        float elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
            now - m_startTime).count();
        float progress = std::clamp(elapsed / m_durationMs, 0.0f, 1.0f);

        float eased = m_curve ? m_curve->getYForX(progress) : progress;
        m_value = lerp(m_start, m_goal, eased);

        if (progress >= 1.0f)
        {
            m_value     = m_goal;
            m_animating = false;
            return false;
        }

        return true;
    }

    T value() const
    {
        return m_value;
    }

    T goal() const
    {
        return m_goal;
    }

    bool isAnimating() const
    {
        return m_animating;
    }

  private:
    T m_value{};
    T m_start{};
    T m_goal{};
    BezierCurve *m_curve = nullptr;
    float m_durationMs   = 300.0f;
    bool m_animating     = false;
    std::chrono::time_point<std::chrono::high_resolution_clock> m_startTime;

    static float lerp(float a, float b, float t)
    {
        return a + (b - a) * t;
    }

    static double lerp(double a, double b, float t)
    {
        return a + (b - a) * t;
    }

    static int lerp(int a, int b, float t)
    {
        return static_cast<int>(a + (b - a) * t);
    }
};

// ============================================================================
// Animated Geometry
// ============================================================================

struct AnimatedGeometry
{
    AnimatedVar<int> x{0};
    AnimatedVar<int> y{0};
    AnimatedVar<int> width{100};
    AnimatedVar<int> height{100};

    void setConfig(BezierCurve *curve, float durationMs)
    {
        x.setConfig(curve, durationMs);
        y.setConfig(curve, durationMs);
        width.setConfig(curve, durationMs);
        height.setConfig(curve, durationMs);
    }

    void setGoal(wf::geometry_t geo, bool animate = true)
    {
        x.set(geo.x, animate);
        y.set(geo.y, animate);
        width.set(geo.width, animate);
        height.set(geo.height, animate);
    }

    void warp(wf::geometry_t geo)
    {
        x.warp(geo.x);
        y.warp(geo.y);
        width.warp(geo.width);
        height.warp(geo.height);
    }

    bool tick()
    {
        bool a = x.tick();
        bool b = y.tick();
        bool c = width.tick();
        bool d = height.tick();
        return a || b || c || d;
    }

    wf::geometry_t current() const
    {
        return {x.value(), y.value(), width.value(), height.value()};
    }

    wf::geometry_t goal() const
    {
        return {x.goal(), y.goal(), width.goal(), height.goal()};
    }

    bool isAnimating() const
    {
        return x.isAnimating() || y.isAnimating() ||
               width.isAnimating() || height.isAnimating();
    }
};

// ============================================================================
// Window Slot for tracking views in overview
// ============================================================================

static const std::string TRANSFORMER_NAME = "wayfire-overview";

struct window_slot_t
{
    wayfire_toplevel_view view;
    wf::geometry_t original_geometry;
    wf::geometry_t target_geometry;
    AnimatedGeometry animated_geometry;
    std::shared_ptr<wf::scene::view_2d_transformer_t> transformer;
    bool is_hovered = false;

    void setConfig(BezierCurve *curve, float durationMs)
    {
        animated_geometry.setConfig(curve, durationMs);
    }

    void startAnimation(bool entering_overview)
    {
        if (entering_overview)
        {
            animated_geometry.warp(original_geometry);
            animated_geometry.setGoal(target_geometry, true);
        } else
        {
            animated_geometry.setGoal(original_geometry, true);
        }
    }

    bool tick()
    {
        return animated_geometry.tick();
    }

    wf::geometry_t currentGeometry() const
    {
        return animated_geometry.current();
    }

    bool isAnimating() const
    {
        return animated_geometry.isAnimating();
    }

    void updateTransformer()
    {
        if (!transformer || !view || !view->is_mapped())
        {
            return;
        }

        wf::geometry_t current  = currentGeometry();
        wf::geometry_t original = original_geometry;

        if ((original.width <= 0) || (original.height <= 0))
        {
            return;
        }

        float scaleX = static_cast<float>(current.width) / original.width;
        float scaleY = static_cast<float>(current.height) / original.height;

        scaleX = std::clamp(scaleX, 0.1f, 10.0f);
        scaleY = std::clamp(scaleY, 0.1f, 10.0f);

        float originalCenterX = original.x + original.width / 2.0f;
        float originalCenterY = original.y + original.height / 2.0f;
        float currentCenterX  = current.x + current.width / 2.0f;
        float currentCenterY  = current.y + current.height / 2.0f;

        float offsetX = currentCenterX - originalCenterX;
        float offsetY = currentCenterY - originalCenterY;

        transformer->translation_x = offsetX;
        transformer->translation_y = offsetY;
        transformer->scale_x = scaleX;
        transformer->scale_y = scaleY;
        transformer->alpha   = is_hovered ? 1.0f : 0.92f;
    }

    void resetTransformer()
    {
        if (!transformer)
        {
            return;
        }

        transformer->translation_x = 0;
        transformer->translation_y = 0;
        transformer->scale_x = 1.0f;
        transformer->scale_y = 1.0f;
        transformer->alpha   = 1.0f;
    }
};

// ============================================================================
// Top Panel
// ============================================================================

class top_panel_t
{
  public:
    wf::output_t *output;
    cairo_surface_t *surface = nullptr;
    cairo_t *cr = nullptr;
    GLuint texture_id = 0;

    int panel_width  = 0;
    int panel_height = 16;
    wf::geometry_t activities_bounds;
    bool activities_hovered = false;
    std::string panel_color = "#1a1a1aE6";

    top_panel_t(wf::output_t *output, int height, const std::string& color) :
        output(output), panel_height(height), panel_color(color)
    {
        create_surface();
    }

    ~top_panel_t()
    {
        destroy_surface();
    }

    void create_surface()
    {
        auto og = output->get_layout_geometry();
        panel_width = og.width;

        surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, panel_width, panel_height);
        cr = cairo_create(surface);
        render_panel();
        upload_texture();
    }

    void destroy_surface()
    {
        if (texture_id)
        {
            wf::gles::run_in_context([&]
            {
                glDeleteTextures(1, &texture_id);
            });
            texture_id = 0;
        }

        if (cr)
        {
            cairo_destroy(cr);
            cr = nullptr;
        }

        if (surface)
        {
            cairo_surface_destroy(surface);
            surface = nullptr;
        }
    }

    void render_panel()
    {
        if (!cr)
        {
            return;
        }

        auto color = color_t::from_hex(panel_color);
        cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
        cairo_set_source_rgba(cr, color.r, color.g, color.b, color.a);
        cairo_paint(cr);

        PangoLayout *layout = pango_cairo_create_layout(cr);
        // Use smaller font for thinner panel
        int font_size = panel_height >= 24 ? 11 : 8;
        char font_str[64];
        snprintf(font_str, sizeof(font_str), "Sans Bold %d", font_size);
        PangoFontDescription *font = pango_font_description_from_string(font_str);
        pango_layout_set_font_description(layout, font);

        pango_layout_set_text(layout, "Activities", -1);
        int text_width, text_height;
        pango_layout_get_pixel_size(layout, &text_width, &text_height);

        int activities_x = 8;
        int activities_y = (panel_height - text_height) / 2;

        activities_bounds = {activities_x - 4, 0, text_width + 8, panel_height};

        if (activities_hovered)
        {
            cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 0.15);
            cairo_rectangle(cr, activities_bounds.x, activities_bounds.y,
                activities_bounds.width, activities_bounds.height);
            cairo_fill(cr);
        }

        cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 1.0);
        cairo_move_to(cr, activities_x, activities_y);
        pango_cairo_show_layout(cr, layout);

        // Clock
        time_t now = time(nullptr);
        struct tm *tm_info = localtime(&now);
        char time_str[64];
        strftime(time_str, sizeof(time_str), "%a %b %d  %H:%M", tm_info);

        pango_layout_set_text(layout, time_str, -1);
        pango_layout_get_pixel_size(layout, &text_width, &text_height);
        cairo_move_to(cr, (panel_width - text_width) / 2, (panel_height - text_height) / 2);
        pango_cairo_show_layout(cr, layout);

        pango_font_description_free(font);
        g_object_unref(layout);
        cairo_surface_flush(surface);
    }

    void upload_texture()
    {
        if (!surface)
        {
            return;
        }

        unsigned char *data = cairo_image_surface_get_data(surface);

        wf::gles::run_in_context([&]
        {
            if (texture_id == 0)
            {
                GL_CALL(glGenTextures(1, &texture_id));
            }

            GL_CALL(glBindTexture(GL_TEXTURE_2D, texture_id));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE));
            GL_CALL(glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, panel_width, panel_height, 0,
                GL_BGRA_EXT, GL_UNSIGNED_BYTE, data));
            GL_CALL(glBindTexture(GL_TEXTURE_2D, 0));
        });
    }

    void set_hover(bool hover)
    {
        if (activities_hovered != hover)
        {
            activities_hovered = hover;
            render_panel();
            upload_texture();
        }
    }

    bool point_in_activities(wf::pointf_t point) const
    {
        auto og     = output->get_layout_geometry();
        int local_x = point.x - og.x;
        int local_y = point.y - og.y;
        return local_x >= activities_bounds.x &&
               local_x < activities_bounds.x + activities_bounds.width &&
               local_y >= activities_bounds.y &&
               local_y < activities_bounds.y + activities_bounds.height;
    }

    GLuint get_texture() const
    {
        return texture_id;
    }
};

// ============================================================================
// Activities View
// ============================================================================

class activities_view_t
{
  public:
    wf::output_t *output;
    std::vector<window_slot_t> window_slots;
    wayfire_toplevel_view hovered_view     = nullptr;
    wf::geometry_t screen_preview_geometry = {0, 0, 0, 0}; // Large main desktop preview

    BezierCurve bezier_curve{0.25f, 0.1f, 0.25f, 1.0f};
    AnimatedVar<float> background_alpha{0.0f};

    // Animated desktop preview geometry - starts fullscreen, shrinks to preview
    AnimatedGeometry desktop_preview_anim;

    // Workspace grid info
    int workspace_rows = 1;
    int workspace_cols = 1;
    wf::point_t current_workspace = {0, 0};
    std::vector<wf::geometry_t> workspace_preview_geometries; // Small thumbnails at top

    // For switching to a different workspace
    bool switching_workspace     = false;
    int pending_workspace_switch = -1;

    bool is_active    = false;
    bool is_animating = false;
    bool transformers_attached = false;

    int corner_radius     = 12;
    double overview_scale = 0.85;
    int spacing = 20;
    int panel_height = 16;
    int animation_duration = 300;

    activities_view_t(wf::output_t *output) : output(output)
    {
        background_alpha.setConfig(&bezier_curve, animation_duration);
        desktop_preview_anim.setConfig(&bezier_curve, animation_duration);
    }

    ~activities_view_t()
    {
        cleanup_transformers();
    }

    void set_config(int radius, double scale, int space, int panel_h, int anim_duration)
    {
        corner_radius  = radius;
        overview_scale = scale;
        spacing = space;
        panel_height = panel_h;
        animation_duration = anim_duration;
        background_alpha.setConfig(&bezier_curve, animation_duration);
        desktop_preview_anim.setConfig(&bezier_curve, animation_duration);
    }

    bool toggle()
    {
        if (is_active)
        {
            deactivate();
        } else
        {
            activate();
        }

        return true;
    }

    void activate()
    {
        if (is_active)
        {
            return;
        }

        is_active    = true;
        is_animating = true;
        transformers_attached = false;

        // Get workspace grid dimensions
        auto wsize = output->wset()->get_workspace_grid_size();
        workspace_cols    = wsize.width;
        workspace_rows    = wsize.height;
        current_workspace = output->wset()->get_current_workspace();

        window_slots.clear();

        auto views = output->wset()->get_views(wf::WSET_MAPPED_ONLY | wf::WSET_CURRENT_WORKSPACE);
        for (auto& view : views)
        {
            auto toplevel = wf::toplevel_cast(view);
            if (toplevel && (toplevel->get_output() == output) &&
                !toplevel->minimized && toplevel->is_mapped())
            {
                window_slot_t slot;
                slot.view = toplevel;
                slot.original_geometry = toplevel->get_geometry();

                if (slot.original_geometry.width <= 0)
                {
                    slot.original_geometry.width = 100;
                }

                if (slot.original_geometry.height <= 0)
                {
                    slot.original_geometry.height = 100;
                }

                window_slots.push_back(slot);
            }
        }

        arrange_windows();
        attach_transformers_and_start_animation();

        // Start desktop preview animation: fullscreen -> large center preview
        auto og = output->get_layout_geometry();
        wf::geometry_t fullscreen = {0, 0, og.width, og.height};
        desktop_preview_anim.warp(fullscreen);
        desktop_preview_anim.setGoal(screen_preview_geometry, true);

        output->render->damage_whole();
        output->render->schedule_redraw();

        LOGI("Overview activated with ", window_slots.size(), " windows");
    }

    void deactivate()
    {
        if (!is_active)
        {
            return;
        }

        is_animating = true;

        // No fade for background
        for (auto& slot : window_slots)
        {
            slot.startAnimation(false);
        }

        // Animate desktop preview back to fullscreen
        auto og = output->get_layout_geometry();
        wf::geometry_t fullscreen = {0, 0, og.width, og.height};
        desktop_preview_anim.setGoal(fullscreen, true);

        output->render->damage_whole();
        output->render->schedule_redraw();
    }

    // Deactivate and switch to a different workspace with zoom animation
    void deactivate_to_workspace(int ws_idx)
    {
        if (!is_active)
        {
            return;
        }

        // Store the target workspace to switch to after animation completes
        pending_workspace_switch = ws_idx;

        // Get the geometry of the target workspace thumbnail
        wf::geometry_t target_geo;
        if ((ws_idx >= 0) && (ws_idx < (int)workspace_preview_geometries.size()))
        {
            target_geo = workspace_preview_geometries[ws_idx];
        } else
        {
            target_geo = screen_preview_geometry;
        }

        // Warp the desktop preview to the target workspace's thumbnail position
        desktop_preview_anim.warp(target_geo);

        is_animating = true;
        switching_workspace = true;

        // Clean up window transformers (we're switching workspaces)
        cleanup_transformers();

        // Animate desktop preview to fullscreen (zoom out from thumbnail)
        auto og = output->get_layout_geometry();
        wf::geometry_t fullscreen = {0, 0, og.width, og.height};
        desktop_preview_anim.setGoal(fullscreen, true);

        output->render->damage_whole();
        output->render->schedule_redraw();
    }

    void cleanup_transformers()
    {
        for (auto& slot : window_slots)
        {
            if (slot.view && slot.view->is_mapped() && slot.transformer)
            {
                slot.resetTransformer();
                slot.view->get_transformed_node()->rem_transformer(TRANSFORMER_NAME);
            }
        }

        window_slots.clear();
        transformers_attached = false;
    }

    void attach_transformers_and_start_animation()
    {
        if (transformers_attached)
        {
            return;
        }

        for (auto& slot : window_slots)
        {
            if (!slot.view || !slot.view->is_mapped())
            {
                continue;
            }

            auto transformer = std::make_shared<wf::scene::view_2d_transformer_t>(slot.view);
            slot.transformer = transformer;

            slot.view->get_transformed_node()->add_transformer(
                transformer, wf::TRANSFORMER_2D, TRANSFORMER_NAME);

            slot.setConfig(&bezier_curve, animation_duration);
            slot.startAnimation(true);
        }

        transformers_attached = true;
        background_alpha.warp(1.0f); // No fade, instant

        LOGI("Transformers attached, animation started");
    }

    void check_animation_done()
    {
        if (!is_animating)
        {
            return;
        }

        bool any_animating = desktop_preview_anim.isAnimating();
        for (auto& slot : window_slots)
        {
            if (slot.isAnimating())
            {
                any_animating = true;
                break;
            }
        }

        if (!any_animating)
        {
            is_animating = false;

            // Check if we're closing (desktop is back to full size)
            auto og = output->get_layout_geometry();
            auto current = desktop_preview_anim.current();
            if (current.width >= og.width - 10) // Close to fullscreen means closing
            {
                // If we were switching workspaces, do it now
                if (switching_workspace && (pending_workspace_switch >= 0))
                {
                    int ws_x = pending_workspace_switch % workspace_cols;
                    int ws_y = pending_workspace_switch / workspace_cols;
                    wf::point_t target_ws = {ws_x, ws_y};
                    output->wset()->set_workspace(target_ws);

                    switching_workspace = false;
                    pending_workspace_switch = -1;
                }

                cleanup_transformers();
                is_active = false;
                background_alpha.warp(0.0f); // Turn off background
                LOGI("Overview deactivated");
            }
        }
    }

    void arrange_windows()
    {
        auto og = output->get_layout_geometry();

        // Small workspace thumbnails at BOTTOM (unchanged from original)
        int total_workspaces = workspace_cols * workspace_rows;
        int thumb_height     = og.height * 0.12;  // Same as original
        int thumb_width    = thumb_height * ((float)og.width / og.height);
        int ws_spacing     = spacing / 2;
        int total_ws_width = total_workspaces * thumb_width + (total_workspaces - 1) * ws_spacing;
        int ws_start_x     = (og.width - total_ws_width) / 2;
        int bottom_margin  = spacing * 2;
        int ws_y = og.height - bottom_margin - thumb_height; // At bottom

        // Store geometry for each small workspace thumbnail
        workspace_preview_geometries.clear();
        for (int i = 0; i < total_workspaces; i++)
        {
            wf::geometry_t ws_geo = {
                ws_start_x + i * (thumb_width + ws_spacing),
                ws_y,
                thumb_width,
                thumb_height
            };
            workspace_preview_geometries.push_back(ws_geo);
        }

        // Large main desktop preview ABOVE the small thumbnails
        int top_margin  = panel_height + spacing * 2;
        int main_bottom = ws_y - spacing * 2; // Stop above the thumbnails
        int main_side_margin = spacing * 4;

        int available_height = main_bottom - top_margin;
        int available_width  = og.width - main_side_margin * 2;

        // Calculate size maintaining aspect ratio
        float screen_aspect = (float)og.width / og.height;
        int main_width  = available_width;
        int main_height = main_width / screen_aspect;

        if (main_height > available_height)
        {
            main_height = available_height;
            main_width  = main_height * screen_aspect;
        }

        // Scale down slightly for visual appeal
        main_width  *= 0.95;
        main_height *= 0.95;

        int main_x = (og.width - main_width) / 2;
        int main_y = top_margin + (available_height - main_height) / 2;

        // This is the large desktop preview geometry (above thumbnails)
        screen_preview_geometry = {main_x, main_y, main_width, main_height};

        if (window_slots.empty())
        {
            return;
        }

        // Arrange windows within the large desktop preview area
        int win_available_width  = main_width - spacing * 4;
        int win_available_height = main_height - spacing * 4;

        int count = window_slots.size();
        int cols  = std::ceil(std::sqrt(count * 1.5));
        int rows  = std::ceil((double)count / cols);

        int cell_width  = (win_available_width - spacing * (cols - 1)) / cols;
        int cell_height = (win_available_height - spacing * (rows - 1)) / rows;

        int grid_width   = cols * cell_width + (cols - 1) * spacing;
        int grid_height  = rows * cell_height + (rows - 1) * spacing;
        int grid_start_x = main_x + (main_width - grid_width) / 2;
        int grid_start_y = main_y + (main_height - grid_height) / 2;

        for (size_t i = 0; i < window_slots.size(); i++)
        {
            auto& slot = window_slots[i];
            int col    = i % cols;
            int row    = i / cols;

            int cell_x = grid_start_x + col * (cell_width + spacing);
            int cell_y = grid_start_y + row * (cell_height + spacing);

            auto& orig = slot.original_geometry;

            double scale_x = (double)cell_width / orig.width;
            double scale_y = (double)cell_height / orig.height;
            double scale   = std::min(scale_x, scale_y) * 0.85;

            int scaled_w = orig.width * scale;
            int scaled_h = orig.height * scale;

            slot.target_geometry = {
                cell_x + (cell_width - scaled_w) / 2,
                cell_y + (cell_height - scaled_h) / 2,
                scaled_w, scaled_h
            };
        }
    }

    void tick_animations()
    {
        background_alpha.tick();
        desktop_preview_anim.tick();

        for (auto& slot : window_slots)
        {
            slot.tick();
            slot.updateTransformer();

            if (slot.view && slot.view->is_mapped())
            {
                slot.view->damage();
            }
        }

        check_animation_done();
        output->render->damage_whole();
    }

    wayfire_toplevel_view find_view_at(wf::pointf_t point)
    {
        for (auto it = window_slots.rbegin(); it != window_slots.rend(); ++it)
        {
            auto geom = it->currentGeometry();
            if ((point.x >= geom.x) && (point.x < geom.x + geom.width) &&
                (point.y >= geom.y) && (point.y < geom.y + geom.height))
            {
                return it->view;
            }
        }

        return nullptr;
    }

    // Find which workspace thumbnail was clicked (-1 if none)
    // Point is in output-local coordinates
    int find_workspace_at(wf::pointf_t point)
    {
        auto og = output->get_layout_geometry();
        // Convert to output-local coordinates
        float local_x = point.x - og.x;
        // Y is inverted in GL coordinates
        float local_y = og.height - (point.y - og.y);

        for (size_t i = 0; i < workspace_preview_geometries.size(); i++)
        {
            auto& geo = workspace_preview_geometries[i];
            if ((local_x >= geo.x) && (local_x < geo.x + geo.width) &&
                (local_y >= geo.y) && (local_y < geo.y + geo.height))
            {
                return (int)i;
            }
        }

        return -1;
    }

    // Check if point is inside the large desktop preview
    bool point_in_large_preview(wf::pointf_t point)
    {
        auto og = output->get_layout_geometry();
        float local_x = point.x - og.x;
        float local_y = og.height - (point.y - og.y);

        auto geo = desktop_preview_anim.current();
        return local_x >= geo.x && local_x < geo.x + geo.width &&
               local_y >= geo.y && local_y < geo.y + geo.height;
    }

    bool handle_click(wf::pointf_t point)
    {
        auto og = output->get_layout_geometry();

        // First check if clicked on a window (windows are in output-local coords)
        wf::pointf_t local_point = {point.x - og.x, point.y - og.y};
        auto view = find_view_at(local_point);
        if (view)
        {
            wf::view_bring_to_front(view);
            deactivate();
            return true;
        }

        // Check if clicked on the large desktop preview (deactivate/zoom back)
        if (point_in_large_preview(point))
        {
            deactivate();
            return true;
        }

        // Check if clicked on a workspace thumbnail
        int ws_idx = find_workspace_at(point);
        if ((ws_idx >= 0) && (ws_idx < (int)workspace_preview_geometries.size()))
        {
            int current_idx = get_current_workspace_index();

            if (ws_idx != current_idx)
            {
                // Animate from the clicked workspace's thumbnail position
                deactivate_to_workspace(ws_idx);
            } else
            {
                // Clicked on current workspace, just deactivate normally
                deactivate();
            }

            return true;
        }

        deactivate();
        return true;
    }

    void update_hover(wf::pointf_t point)
    {
        auto new_hovered = find_view_at(point);

        if (new_hovered != hovered_view)
        {
            for (auto& slot : window_slots)
            {
                slot.is_hovered = (slot.view == new_hovered);
            }

            hovered_view = new_hovered;
        }
    }

    bool currently_animating() const
    {
        return is_animating;
    }

    float get_background_alpha() const
    {
        return background_alpha.value();
    }

    wf::geometry_t get_screen_preview_geometry() const
    {
        return screen_preview_geometry;
    }

    wf::geometry_t get_current_desktop_geometry() const
    {
        return desktop_preview_anim.current();
    }

    const std::vector<wf::geometry_t>& get_workspace_geometries() const
    {
        return workspace_preview_geometries;
    }

    int get_current_workspace_index() const
    {
        return current_workspace.y * workspace_cols + current_workspace.x;
    }

    int get_total_workspaces() const
    {
        return workspace_cols * workspace_rows;
    }

    // Returns the workspace index that should be drawn as the animating one
    int get_animating_workspace_index() const
    {
        if (switching_workspace && (pending_workspace_switch >= 0))
        {
            return pending_workspace_switch;
        }

        return current_workspace.y * workspace_cols + current_workspace.x;
    }
};

// ============================================================================
// Overview Render Node - renders the overview background and preview
// ============================================================================

class overview_render_node_t : public wf::scene::node_t
{
  public:
    wf::output_t *output;
    activities_view_t *activities;
    OpenGL::program_t *tex_program;
    OpenGL::program_t *rounded_tex_program;
    OpenGL::program_t *col_program;
    GLuint wallpaper_texture;

    // Workspace capture data structure (public so render_overview can use it)
    struct workspace_capture_t
    {
        std::shared_ptr<wf::workspace_stream_node_t> stream;
        std::vector<wf::scene::render_instance_uptr> instances;
        wf::region_t damage;
        wf::auxilliary_buffer_t framebuffer;
        wf::point_t workspace;
    };

    class overview_render_instance_t : public wf::scene::render_instance_t
    {
        std::shared_ptr<overview_render_node_t> self;
        wf::scene::damage_callback push_damage;

      public:
        // Workspace streams for ALL workspaces
        std::vector<workspace_capture_t> workspace_captures;

      private:
        wf::signal::connection_t<wf::scene::node_damage_signal> on_node_damage =
            [=] (wf::scene::node_damage_signal *ev)
        {
            push_damage(ev->region);
        };

      public:
        overview_render_instance_t(overview_render_node_t *self,
            wf::scene::damage_callback push_damage)
        {
            this->self = std::dynamic_pointer_cast<overview_render_node_t>(self->shared_from_this());
            this->push_damage = push_damage;
            self->connect(&on_node_damage);

            // Create workspace streams for ALL workspaces
            auto wsize = self->output->wset()->get_workspace_grid_size();

            for (int y = 0; y < wsize.height; y++)
            {
                for (int x = 0; x < wsize.width; x++)
                {
                    wf::point_t ws = {x, y};
                    workspace_capture_t capture;
                    capture.workspace = ws;
                    capture.stream    = std::make_shared<wf::workspace_stream_node_t>(self->output, ws);

                    auto push_damage_child =
                        [this, self, idx = workspace_captures.size()] (const wf::region_t& damage)
                    {
                        if (idx < workspace_captures.size())
                        {
                            workspace_captures[idx].damage |= damage;
                        }

                        this->push_damage(self->get_bounding_box());
                    };

                    capture.stream->gen_render_instances(capture.instances, push_damage_child, self->output);
                    capture.damage |= capture.stream->get_bounding_box();

                    workspace_captures.push_back(std::move(capture));
                }
            }
        }

        void schedule_instructions(
            std::vector<wf::scene::render_instruction_t>& instructions,
            const wf::render_target_t& target, wf::region_t& damage) override
        {
            auto bbox = self->get_bounding_box();
            const float scale = self->output->handle->scale;

            // Render ALL workspaces to their framebuffers
            for (auto& capture : workspace_captures)
            {
                auto ws_bbox = capture.stream->get_bounding_box();
                capture.framebuffer.allocate(wf::dimensions(ws_bbox), scale);

                wf::render_target_t ws_target{capture.framebuffer};
                ws_target.geometry = ws_bbox;
                ws_target.scale    = scale;

                wf::render_pass_params_t params;
                params.instances = &capture.instances;
                params.damage    = capture.damage;
                params.reference_output = self->output;
                params.target = ws_target;
                params.flags  = wf::RPASS_CLEAR_BACKGROUND | wf::RPASS_EMIT_SIGNALS;

                wf::render_pass_t::run(params);
                capture.damage.clear();
            }

            // Schedule our render instruction
            instructions.push_back(wf::scene::render_instruction_t{
                        .instance = this,
                        .target   = target,
                        .damage   = damage & bbox,
                    });

            damage ^= bbox;
        }

        void render(const wf::scene::render_instruction_t& data) override
        {
            self->render_overview(data, workspace_captures);
        }

        void compute_visibility(wf::output_t *output, wf::region_t&) override
        {
            for (auto& capture : workspace_captures)
            {
                for (auto& ch : capture.instances)
                {
                    wf::region_t ws_region = capture.stream->get_bounding_box();
                    ch->compute_visibility(output, ws_region);
                }
            }
        }
    };

    overview_render_node_t(wf::output_t *output, activities_view_t *activities,
        OpenGL::program_t *tex_prog, OpenGL::program_t *rounded_tex_prog,
        OpenGL::program_t *col_prog, GLuint wallpaper_tex) :
        node_t(false), output(output), activities(activities),
        tex_program(tex_prog), rounded_tex_program(rounded_tex_prog), col_program(col_prog),
        wallpaper_texture(wallpaper_tex)
    {}

    void gen_render_instances(
        std::vector<wf::scene::render_instance_uptr>& instances,
        wf::scene::damage_callback push_damage, wf::output_t *shown_on) override
    {
        if (shown_on != output)
        {
            return;
        }

        instances.push_back(std::make_unique<overview_render_instance_t>(this, push_damage));
    }

    wf::geometry_t get_bounding_box() override
    {
        return output->get_layout_geometry();
    }

    void render_texture(const wf::render_target_t& target, GLuint tex,
        wf::geometry_t box, float alpha = 1.0f, bool invert_y = false)
    {
        auto og = output->get_layout_geometry();
        glm::mat4 ortho = glm::ortho<float>(og.x, og.x + og.width, og.y + og.height, og.y, -1, 1);

        float x1 = box.x;
        float x2 = box.x + box.width;
        float y1 = box.y;
        float y2 = box.y + box.height;

        float v0 = invert_y ? 1.0f : 0.0f;
        float v1 = invert_y ? 0.0f : 1.0f;

        // Separate position and UV arrays like cube plugin does
        GLfloat vertexData[] = {
            x1, y1,
            x2, y1,
            x2, y2,
            x1, y2,
        };

        GLfloat coordData[] = {
            0.0f, v0,
            1.0f, v0,
            1.0f, v1,
            0.0f, v1,
        };

        tex_program->use(wf::TEXTURE_TYPE_RGBA);
        tex_program->uniformMatrix4f("matrix", ortho);
        tex_program->uniform1i("smp", 0);
        tex_program->uniform1f("alpha", alpha);

        GL_CALL(glActiveTexture(GL_TEXTURE0));
        GL_CALL(glBindTexture(GL_TEXTURE_2D, tex));

        // Use same signature as cube: attrib_pointer(name, size, stride, data)
        tex_program->attrib_pointer("position", 2, 0, vertexData);
        tex_program->attrib_pointer("uv", 2, 0, coordData);

        GL_CALL(glEnable(GL_BLEND));
        GL_CALL(glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA));
        GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 4));
        GL_CALL(glDisable(GL_BLEND));

        GL_CALL(glBindTexture(GL_TEXTURE_2D, 0));
        tex_program->deactivate();
    }

    void render_solid_rect(const wf::render_target_t& target, wf::geometry_t box, glm::vec4 color)
    {
        auto og = output->get_layout_geometry();
        glm::mat4 ortho = glm::ortho<float>(og.x, og.x + og.width, og.y + og.height, og.y, -1, 1);

        float x1 = box.x;
        float x2 = box.x + box.width;
        float y1 = box.y;
        float y2 = box.y + box.height;

        GLfloat verts[] = {
            x1, y1,
            x2, y1,
            x2, y2,
            x1, y2,
        };

        col_program->use(wf::TEXTURE_TYPE_RGBA);
        col_program->uniformMatrix4f("matrix", ortho);
        col_program->uniform4f("color", color);

        // Use same signature as cube: attrib_pointer(name, size, stride, data)
        col_program->attrib_pointer("position", 2, 0, verts);

        GL_CALL(glEnable(GL_BLEND));
        GL_CALL(glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA));
        GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 4));
        GL_CALL(glDisable(GL_BLEND));

        col_program->deactivate();
    }

    void render_rounded_texture(const wf::render_target_t& target, GLuint tex,
        wf::geometry_t box, float alpha, float radius, bool invert_y = false)
    {
        auto og = output->get_layout_geometry();
        glm::mat4 ortho = glm::ortho<float>(og.x, og.x + og.width, og.y + og.height, og.y, -1, 1);

        float x1 = box.x;
        float x2 = box.x + box.width;
        float y1 = box.y;
        float y2 = box.y + box.height;

        float v0 = invert_y ? 1.0f : 0.0f;
        float v1 = invert_y ? 0.0f : 1.0f;

        GLfloat vertexData[] = {
            x1, y1,
            x2, y1,
            x2, y2,
            x1, y2,
        };

        GLfloat coordData[] = {
            0.0f, v0,
            1.0f, v0,
            1.0f, v1,
            0.0f, v1,
        };

        rounded_tex_program->use(wf::TEXTURE_TYPE_RGBA);
        rounded_tex_program->uniformMatrix4f("matrix", ortho);
        rounded_tex_program->uniform1i("smp", 0);
        rounded_tex_program->uniform1f("alpha", alpha);
        rounded_tex_program->uniform1f("radius", radius);
        rounded_tex_program->uniform2f("size", (float)box.width, (float)box.height);

        GL_CALL(glActiveTexture(GL_TEXTURE0));
        GL_CALL(glBindTexture(GL_TEXTURE_2D, tex));

        rounded_tex_program->attrib_pointer("position", 2, 0, vertexData);
        rounded_tex_program->attrib_pointer("uv", 2, 0, coordData);

        GL_CALL(glEnable(GL_BLEND));
        GL_CALL(glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA));
        GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 4));
        GL_CALL(glDisable(GL_BLEND));

        GL_CALL(glBindTexture(GL_TEXTURE_2D, 0));
        rounded_tex_program->deactivate();
    }

    void render_overview(const wf::scene::render_instruction_t& data,
        std::vector<workspace_capture_t>& workspace_captures)
    {
        data.pass->custom_gles_subpass([&]
        {
            auto og     = output->get_layout_geometry();
            float alpha = activities->get_background_alpha();

            if (alpha < 0.01f)
            {
                return;
            }

            // Clear first
            GL_CALL(glClearColor(0.0f, 0.0f, 0.0f, 1.0f));
            GL_CALL(glClear(GL_COLOR_BUFFER_BIT));

            float corner_r = activities->corner_radius;
            int animating_ws_idx = activities->get_animating_workspace_index();

            // Draw wallpaper as background
            if (wallpaper_texture != 0)
            {
                wf::geometry_t bg_geo = {og.x, og.y, og.width, og.height};
                render_texture(data.target, wallpaper_texture, bg_geo, 1.0f, true);

                // Draw dark semi-transparent overlay
                render_solid_rect(data.target, bg_geo, glm::vec4(0.0f, 0.0f, 0.0f, 0.5f));
            }

            // Draw ALL small workspace thumbnails at BOTTOM (including current)
            auto& ws_geometries = activities->get_workspace_geometries();
            for (size_t i = 0; i < ws_geometries.size() && i < workspace_captures.size(); i++)
            {
                wf::geometry_t ws_geo = ws_geometries[i];
                ws_geo.x += og.x;
                ws_geo.y += og.y;

                // Current workspace thumbnail is slightly brighter
                float thumb_alpha = ((int)i == animating_ws_idx) ? 1.0f : 0.7f;

                auto tex = wf::gles_texture_t::from_aux(workspace_captures[i].framebuffer);
                render_rounded_texture(data.target, tex.tex_id, ws_geo, thumb_alpha, corner_r * 0.5f, true);
            }

            // Draw the large main desktop preview ABOVE the thumbnails
            auto desktop_geo = activities->get_current_desktop_geometry();
            desktop_geo.x   += og.x;
            desktop_geo.y   += og.y;

            if ((desktop_geo.width > 0) && (desktop_geo.height > 0) &&
                (animating_ws_idx >= 0) && (animating_ws_idx < (int)workspace_captures.size()))
            {
                // Calculate corner radius that scales with the animation
                // Use larger base radius for more curved corners
                float large_corner_r = corner_r * 2.0f;
                float scale_factor   = (float)desktop_geo.width / og.width;
                float current_radius = large_corner_r * (1.0f - scale_factor);
                current_radius = std::max(0.0f, std::min(current_radius, large_corner_r));

                auto tex = wf::gles_texture_t::from_aux(workspace_captures[animating_ws_idx].framebuffer);
                if (current_radius > 1.0f)
                {
                    render_rounded_texture(data.target, tex.tex_id, desktop_geo, 1.0f, current_radius, true);
                } else
                {
                    render_texture(data.target, tex.tex_id, desktop_geo, 1.0f, true);
                }
            }
        });
    }
};

// ============================================================================
// Per-output Instance
// ============================================================================

class overview_output_t : public wf::per_output_plugin_instance_t
{
  public:
    std::unique_ptr<top_panel_t> panel;
    std::unique_ptr<activities_view_t> activities;
    std::shared_ptr<overview_render_node_t> render_node;

    wf::activator_callback toggle_cb;
    wf::wl_timer<false> clock_timer;
    wf::effect_hook_t pre_hook;
    wf::effect_hook_t post_hook;

    OpenGL::program_t tex_program;
    OpenGL::program_t rounded_tex_program;
    OpenGL::program_t col_program;
    bool programs_loaded = false;

    // Wallpaper texture
    GLuint wallpaper_texture = 0;
    int wallpaper_width  = 0;
    int wallpaper_height = 0;
    std::string wallpaper_path = "/home/light/Pictures/wallpapers/Dynamic-Wallpapers/Light/Beach_light.png";

    int panel_height = 16;
    std::string panel_color = "#1a1a1aE6";
    int corner_radius = 12;
    int animation_duration = 300;
    double overview_scale  = 0.85;
    int spacing = 20;

    void load_wallpaper()
    {
        cairo_surface_t *img = cairo_image_surface_create_from_png(wallpaper_path.c_str());
        if (cairo_surface_status(img) != CAIRO_STATUS_SUCCESS)
        {
            LOGE("Failed to load wallpaper from: ", wallpaper_path);
            cairo_surface_destroy(img);
            return;
        }

        wallpaper_width  = cairo_image_surface_get_width(img);
        wallpaper_height = cairo_image_surface_get_height(img);
        unsigned char *data = cairo_image_surface_get_data(img);

        wf::gles::run_in_context([&]
        {
            if (wallpaper_texture == 0)
            {
                GL_CALL(glGenTextures(1, &wallpaper_texture));
            }

            GL_CALL(glBindTexture(GL_TEXTURE_2D, wallpaper_texture));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE));
            GL_CALL(glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE));
            GL_CALL(glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, wallpaper_width, wallpaper_height, 0,
                GL_BGRA_EXT, GL_UNSIGNED_BYTE, data));
            GL_CALL(glBindTexture(GL_TEXTURE_2D, 0));
        });

        cairo_surface_destroy(img);
        LOGI("Loaded wallpaper: ", wallpaper_width, "x", wallpaper_height);
    }

    void init() override
    {
        panel = std::make_unique<top_panel_t>(output, panel_height, panel_color);
        activities = std::make_unique<activities_view_t>(output);
        activities->set_config(corner_radius, overview_scale, spacing, panel_height, animation_duration);

        // Load shader programs and wallpaper
        wf::gles::run_in_context([&]
        {
            load_programs();
        });
        load_wallpaper();

        // PRE hook for animation ticking and managing render node
        pre_hook = [this] ()
        {
            if (activities->is_active || activities->is_animating)
            {
                // Add render node if not present
                if (!render_node)
                {
                    render_node = std::make_shared<overview_render_node_t>(
                        output, activities.get(), &tex_program, &rounded_tex_program, &col_program,
                        wallpaper_texture);
                    wf::scene::add_front(wf::get_core().scene(), render_node);
                }

                activities->tick_animations();

                if (render_node)
                {
                    wf::scene::damage_node(render_node, render_node->get_bounding_box());
                }
            } else if (render_node)
            {
                // Remove render node when not active
                wf::scene::remove_child(render_node);
                render_node = nullptr;
            }
        };
        output->render->add_effect(&pre_hook, wf::OUTPUT_EFFECT_PRE);

        // POST hook for panel overlay
        post_hook = [this] () { render_panel_overlay(); };
        output->render->add_effect(&post_hook, wf::OUTPUT_EFFECT_OVERLAY);

        // Clock update timer
        clock_timer.set_timeout(60000, [this] ()
        {
            panel->render_panel();
            panel->upload_texture();
            output->render->damage_whole();
            return true;
        });

        LOGI("Wayfire Overview output initialized");
    }

    void load_programs()
    {
        if (programs_loaded)
        {
            return;
        }

        programs_loaded = true;

        const char *tex_vert_src =
            "#version 100\n"
            "attribute mediump vec2 position;\n"
            "attribute mediump vec2 uv;\n"
            "varying mediump vec2 uvpos;\n"
            "uniform mat4 matrix;\n"
            "void main() {\n"
            "   gl_Position = matrix * vec4(position, 0.0, 1.0);\n"
            "   uvpos = uv;\n"
            "}\n";

        const char *tex_frag_src =
            "#version 100\n"
            "precision mediump float;\n"
            "varying mediump vec2 uvpos;\n"
            "uniform sampler2D smp;\n"
            "uniform float alpha;\n"
            "void main() {\n"
            "   vec4 c = texture2D(smp, uvpos);\n"
            "   gl_FragColor = vec4(c.rgb * alpha, c.a * alpha);\n"
            "}\n";

        tex_program.compile(tex_vert_src, tex_frag_src);

        // Rounded corner texture shader
        const char *rounded_tex_vert_src =
            "#version 100\n"
            "precision mediump float;\n"
            "attribute vec2 position;\n"
            "attribute vec2 uv;\n"
            "varying vec2 uvpos;\n"
            "varying vec2 fragCoord;\n"
            "uniform mat4 matrix;\n"
            "uniform vec2 size;\n"
            "void main() {\n"
            "   gl_Position = matrix * vec4(position, 0.0, 1.0);\n"
            "   uvpos = uv;\n"
            "   fragCoord = uv * size;\n"
            "}\n";

        const char *rounded_tex_frag_src =
            "#version 100\n"
            "precision mediump float;\n"
            "varying vec2 uvpos;\n"
            "varying vec2 fragCoord;\n"
            "uniform sampler2D smp;\n"
            "uniform float alpha;\n"
            "uniform float radius;\n"
            "uniform vec2 size;\n"
            "void main() {\n"
            "   vec4 c = texture2D(smp, uvpos);\n"
            "   vec2 pos = fragCoord;\n"
            "   vec2 cornerDist;\n"
            "   if (pos.x < radius && pos.y < radius) {\n"
            "       cornerDist = pos - vec2(radius, radius);\n"
            "   } else if (pos.x > size.x - radius && pos.y < radius) {\n"
            "       cornerDist = pos - vec2(size.x - radius, radius);\n"
            "   } else if (pos.x < radius && pos.y > size.y - radius) {\n"
            "       cornerDist = pos - vec2(radius, size.y - radius);\n"
            "   } else if (pos.x > size.x - radius && pos.y > size.y - radius) {\n"
            "       cornerDist = pos - vec2(size.x - radius, size.y - radius);\n"
            "   } else {\n"
            "       gl_FragColor = vec4(c.rgb * alpha, c.a * alpha);\n"
            "       return;\n"
            "   }\n"
            "   float dist = length(cornerDist);\n"
            "   float aa = smoothstep(radius, radius - 1.5, dist);\n"
            "   gl_FragColor = vec4(c.rgb * alpha * aa, c.a * alpha * aa);\n"
            "}\n";

        rounded_tex_program.compile(rounded_tex_vert_src, rounded_tex_frag_src);

        const char *col_vert_src =
            "#version 100\n"
            "attribute mediump vec2 position;\n"
            "uniform mat4 matrix;\n"
            "void main() {\n"
            "   gl_Position = matrix * vec4(position, 0.0, 1.0);\n"
            "}\n";

        const char *col_frag_src =
            "#version 100\n"
            "precision mediump float;\n"
            "uniform vec4 color;\n"
            "void main() {\n"
            "   gl_FragColor = color;\n"
            "}\n";

        col_program.compile(col_vert_src, col_frag_src);
    }

    void render_panel_overlay()
    {
        auto og = output->get_layout_geometry();

        GLuint panel_tex = panel->get_texture();
        if (panel_tex)
        {
            // Create orthographic projection
            glm::mat4 ortho = glm::ortho<float>(og.x, og.x + og.width, og.y + og.height, og.y, -1, 1);

            // Panel at BOTTOM of screen in GL coordinates (og.height - panel_height)
            // but visually at TOP because of Y-flip
            wf::geometry_t panel_geom =
            {og.x, og.y + og.height - panel->panel_height, panel->panel_width, panel->panel_height};

            float x1 = panel_geom.x;
            float x2 = panel_geom.x + panel_geom.width;
            float y1 = panel_geom.y;
            float y2 = panel_geom.y + panel_geom.height;

            // Separate arrays like cube plugin
            GLfloat vertexData[] = {
                x1, y1,
                x2, y1,
                x2, y2,
                x1, y2,
            };

            // Invert Y for texture (1,0 at top, 0,1 at bottom)
            GLfloat coordData[] = {
                0.0f, 1.0f,
                1.0f, 1.0f,
                1.0f, 0.0f,
                0.0f, 0.0f,
            };

            tex_program.use(wf::TEXTURE_TYPE_RGBA);
            tex_program.uniformMatrix4f("matrix", ortho);
            tex_program.uniform1i("smp", 0);
            tex_program.uniform1f("alpha", 1.0f);

            GL_CALL(glActiveTexture(GL_TEXTURE0));
            GL_CALL(glBindTexture(GL_TEXTURE_2D, panel_tex));

            tex_program.attrib_pointer("position", 2, 0, vertexData);
            tex_program.attrib_pointer("uv", 2, 0, coordData);

            GL_CALL(glEnable(GL_BLEND));
            GL_CALL(glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA));
            GL_CALL(glDrawArrays(GL_TRIANGLE_FAN, 0, 4));
            GL_CALL(glDisable(GL_BLEND));

            GL_CALL(glBindTexture(GL_TEXTURE_2D, 0));
            tex_program.deactivate();
        }

        // Schedule redraw if animating
        if (activities->currently_animating())
        {
            output->render->schedule_redraw();
        }
    }

    void fini() override
    {
        if (render_node)
        {
            wf::scene::remove_child(render_node);
            render_node = nullptr;
        }

        output->render->rem_effect(&pre_hook);
        output->render->rem_effect(&post_hook);
        clock_timer.disconnect();

        wf::gles::run_in_context_if_gles([&]
        {
            tex_program.free_resources();
            rounded_tex_program.free_resources();
            col_program.free_resources();
            if (wallpaper_texture)
            {
                glDeleteTextures(1, &wallpaper_texture);
                wallpaper_texture = 0;
            }
        });

        activities.reset();
        panel.reset();
    }

    void handle_motion(wf::pointf_t cursor)
    {
        bool was_hovered = panel->activities_hovered;
        panel->set_hover(panel->point_in_activities(cursor));

        if (activities->is_active && !activities->currently_animating())
        {
            activities->update_hover(cursor);
            output->render->damage_whole();
        }

        if (was_hovered != panel->activities_hovered)
        {
            output->render->damage_whole();
        }
    }

    bool handle_button(uint32_t button, uint32_t state, wf::pointf_t cursor)
    {
        if ((button != BTN_LEFT) || (state != WL_POINTER_BUTTON_STATE_PRESSED))
        {
            return false;
        }

        if (panel->point_in_activities(cursor))
        {
            activities->toggle();
            output->render->damage_whole();
            return true;
        }

        if (activities->is_active && !activities->currently_animating())
        {
            bool handled = activities->handle_click(cursor);
            output->render->damage_whole();
            return handled;
        }

        return false;
    }
};

// ============================================================================
// Main Plugin
// ============================================================================

class wayfire_overview_t : public wf::plugin_interface_t
{
  public:
    wf::option_wrapper_t<int> opt_panel_height{"overview/panel_height"};
    wf::option_wrapper_t<std::string> opt_panel_color{"overview/panel_color"};
    wf::option_wrapper_t<int> opt_corner_radius{"overview/corner_radius"};
    wf::option_wrapper_t<int> opt_animation_duration{"overview/animation_duration"};
    wf::option_wrapper_t<double> opt_overview_scale{"overview/overview_scale"};
    wf::option_wrapper_t<int> opt_spacing{"overview/spacing"};
    wf::option_wrapper_t<wf::activatorbinding_t> opt_toggle{"overview/toggle"};
    wf::option_wrapper_t<std::string> opt_wallpaper{"overview/wallpaper"};

    std::map<wf::output_t*, std::unique_ptr<overview_output_t>> outputs;

    wf::signal::connection_t<wf::output_added_signal> on_output_added =
        [this] (wf::output_added_signal *ev) { add_output(ev->output); };
    wf::signal::connection_t<wf::output_removed_signal> on_output_removed =
        [this] (wf::output_removed_signal *ev) { remove_output(ev->output); };
    wf::signal::connection_t<wf::post_input_event_signal<wlr_pointer_motion_event>>
    on_motion = [this] (wf::post_input_event_signal<wlr_pointer_motion_event>*) { handle_motion(); };
    wf::signal::connection_t<wf::post_input_event_signal<wlr_pointer_button_event>>
    on_button = [this] (wf::post_input_event_signal<wlr_pointer_button_event> *ev)
    {
        handle_button(ev->event);
    };

    void init() override
    {
        wf::get_core().connect(&on_output_added);
        wf::get_core().connect(&on_output_removed);
        wf::get_core().connect(&on_motion);
        wf::get_core().connect(&on_button);

        for (auto& output : wf::get_core().output_layout->get_outputs())
        {
            add_output(output);
        }

        LOGI("Wayfire Overview plugin initialized");
    }

    void fini() override
    {
        for (auto& [out, inst] : outputs)
        {
            out->rem_binding(&inst->toggle_cb);
            inst->fini();
        }

        outputs.clear();
        LOGI("Wayfire Overview plugin finalized");
    }

    void add_output(wf::output_t *output)
    {
        auto inst = std::make_unique<overview_output_t>();
        inst->panel_height  = opt_panel_height;
        inst->panel_color   = (std::string)opt_panel_color;
        inst->corner_radius = opt_corner_radius;
        inst->animation_duration = opt_animation_duration;
        inst->overview_scale     = opt_overview_scale;
        inst->spacing = opt_spacing;
        inst->output  = output;

        std::string wp = (std::string)opt_wallpaper;
        if (!wp.empty())
        {
            inst->wallpaper_path = wp;
        }

        inst->init();

        auto *ptr = inst.get();
        inst->toggle_cb = [ptr] (auto)
        {
            ptr->activities->toggle();
            ptr->output->render->damage_whole();
            return true;
        };
        output->add_activator(opt_toggle, &inst->toggle_cb);

        outputs[output] = std::move(inst);
    }

    void remove_output(wf::output_t *output)
    {
        if (outputs.count(output))
        {
            output->rem_binding(&outputs[output]->toggle_cb);
            outputs[output]->fini();
            outputs.erase(output);
        }
    }

    void handle_motion()
    {
        auto cursor = wf::get_core().get_cursor_position();
        auto output = wf::get_core().output_layout->get_output_at(cursor.x, cursor.y);
        if (output && outputs.count(output))
        {
            outputs[output]->handle_motion(cursor);
        }
    }

    void handle_button(wlr_pointer_button_event *event)
    {
        auto cursor = wf::get_core().get_cursor_position();
        auto output = wf::get_core().output_layout->get_output_at(cursor.x, cursor.y);
        if (output && outputs.count(output))
        {
            outputs[output]->handle_button(event->button, event->state, cursor);
        }
    }
};
} // namespace overview
} // namespace wf

DECLARE_WAYFIRE_PLUGIN(wf::overview::wayfire_overview_t);
