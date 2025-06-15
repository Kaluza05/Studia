(* # -*- mode: c; -*- *)

let with_runtime p =
  Printf.sprintf
  {|
#define STACK_SIZE 10000
#define HEAP_SIZE  1000000

#include <stdio.h>

int stack[STACK_SIZE];
int stack_ptr = -1; // top-most element on the stack

int heap[HEAP_SIZE];
int heap_ptr = 0; // first free cell

// heap objects
#define INT  0
#define BOOL 1
#define UNIT 2
#define PAIR 3

void print_heap_obj(int ptr)
{
  switch (heap[ptr-1])
  {
    case INT:
      printf("%%d", heap[ptr]);
      break;
    case BOOL:
      if (heap[ptr])
        printf("true");
      else
        printf("false");
      break;
    case UNIT:
      printf("()");
      break;
    case PAIR:
      printf("(");
      print_heap_obj(heap[ptr]);
      printf(",");
      print_heap_obj(heap[ptr+1]);
      printf(")");
  }

int structural_comare(int ptr1, int ptr2)
{
  int tag1 = heap[ptr1 - 1];
  int tag2 = heap[ptr2 - 1];
  if (tag1 != tag2) {
    0;
    return
  }
  switch tag1 { 
    case INT:
      if (heap[prt1] == heap[ptr2]) {
        return 1
      }
      return 0
      break;
    case BOOL:
      if (heap[prt1] == heap[ptr2]) {
        return 1
      }
      return 0
      break;
    case UNIT:
      return 1;
      break;
    case PAIR:
    return structural_compare(heap[ptr1], heap[ptr2]) && structural_compare(heap[ptr1 + 1], heap[ptr2 + 1]);
    break
  }
}

int main()
{
%s
  print_heap_obj(stack[0]);
  printf("\n");
  return 0;
}
  |}
  p
