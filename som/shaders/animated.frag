#version 450 core

in vec2 tex_coord;

uniform sampler2D diffuse_map;

out vec4 fragment_color;

void main () {
  fragment_color = texture (diffuse_map, tex_coord);
}
