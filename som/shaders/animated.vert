#version 450 core

const int JOINTS = 30;

layout (location = 0) in vec3 a_position;
layout (location = 1) in vec3 a_normal;
layout (location = 2) in vec2 a_tex_coord;
layout (location = 3) in vec4 a_joints;
layout (location = 4) in vec4 a_weights;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat4 joint_transformations [JOINTS];
uniform mat4 joint_ibms [JOINTS];


out vec2 tex_coord;

void main () {
  vec4 position = vec4 (a_position, 1.0);

  mat4 joints [JOINTS];

  for (int i = 0; i < JOINTS; i++) {
    joints [i] = joint_transformations [i] * joint_ibms [i];
  }

  mat4 skin = a_weights.x * joints [int (a_joints.x)]
            + a_weights.y * joints [int (a_joints.y)]
            + a_weights.z * joints [int (a_joints.z)]
            + a_weights.w * joints [int (a_joints.w)];

  tex_coord = a_tex_coord;
  gl_Position = projection * view * model * skin * position;
}
