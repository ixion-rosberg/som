#version 450 core

in vec2 fragment_position;
in vec2 tex_coord;

uniform sampler2D diffuse_map;
uniform float max;

out vec4 fragment_color;

void main () {

  if (fragment_position.x > max)
    discard;

  fragment_color = texture (diffuse_map, tex_coord);
}
