#version 450 core

layout (location = 0) in vec3 a_position;
layout (location = 1) in vec3 a_normal;
layout (location = 2) in vec2 a_tex_coord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec2 tex_coord;

void main () {
  vec4 position = vec4 (a_position, 1.0);

  tex_coord = a_tex_coord;
  gl_Position = projection * view * model * position;
}
