#pragma once

#include <stdint.h>

typedef struct {
  char const *data;
  intptr_t    len;
} Hep_String;

typedef struct {
  intptr_t line, column, offset;
} Hep_Location;

typedef struct {
  Hep_Location start, end;
  Hep_String   message;
} Hep_Error;

typedef struct {
  Hep_Error *errors;
  intptr_t   n_errors;
  uint32_t  *instructions;
  intptr_t   n_instructions;
  void      *_error_arena;
} Hep_Result;

Hep_Result hep_compile_shader(char const *source, char const *path);
void       hep_result_free(Hep_Result);
