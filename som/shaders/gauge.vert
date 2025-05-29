#version 450 core

layout (location = 0) in vec2 a_position;
layout (location = 1) in vec2 a_tex_coord;

uniform mat4 projection;

out vec2 fragment_position;
out vec2 tex_coord;

void main () {
  fragment_position = a_position;
  tex_coord = a_tex_coord;

  gl_Position = projection * vec4 (a_position, 0, 1);
}
