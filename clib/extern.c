/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Structured output */

#include <stdio.h>
#include <string.h>
#include "alloc.h"
#include "fail.h"
#include "gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "reverse.h"
#include "str.h"

#include "inextern.h"
#include "print_symbols.h"

/* To keep track of sharing in externed objects */

typedef unsigned long byteoffset_t;

struct extern_obj {
  byteoffset_t ofs;
  value obj;
};

static byteoffset_t initial_ofs = 1; /* Initial value of object offsets */
static struct extern_obj * extern_table = NULL;
static unsigned long extern_table_size;
static byteoffset_t obj_counter;    /* Number of objects emitted so far */

#ifdef ARCH_SIXTYFOUR
#define Hash(v) (((unsigned long) ((v) >> 3)) % extern_table_size)
#else
#define Hash(v) (((unsigned long) ((v) >> 2)) % extern_table_size)
#endif

/* Allocate a new extern table */
static void alloc_extern_table(void)
{
  asize_t i;
  extern_table = (struct extern_obj *)
                 stat_alloc(extern_table_size * sizeof(struct extern_obj));
  for (i = 0; i < extern_table_size; i++) extern_table[i].ofs = 0;
}

/* Grow the extern table */
static void resize_extern_table(void)
{
  asize_t oldsize;
  struct extern_obj * oldtable;
  value obj;
  byteoffset_t ofs;
  asize_t i, h;

  oldsize = extern_table_size;
  oldtable = extern_table;
  extern_table_size = 2 * extern_table_size;
  alloc_extern_table();
  for (i = 0; i < oldsize; i++) {
    ofs = oldtable[i].ofs;
    if (ofs >= initial_ofs) {
      obj = oldtable[i].obj;
      h = Hash(obj);
      while (extern_table[h].ofs >= initial_ofs) {
        h++;
        if (h >= extern_table_size) h = 0;
      }
      extern_table[h].ofs = ofs;
      extern_table[h].obj = obj;
    }
  }
  stat_free(oldtable);
}

/* Free the extern table. We keep it around for next call if
   it's still small (we did not grow it) and the initial offset
   does not risk running over next time. */
static void free_extern_table(void)
{
  if (extern_table_size > INITIAL_EXTERN_TABLE_SIZE ||
      initial_ofs >= INITIAL_OFFSET_MAX) {
    stat_free(extern_table);
    extern_table = NULL;
  }
}

/* To buffer the output */

static char * extern_block, * extern_ptr, * extern_limit;
static int extern_block_malloced;

static void alloc_extern_block(void)
{
  extern_block = stat_alloc(INITIAL_EXTERN_BLOCK_SIZE);
  extern_limit = extern_block + INITIAL_EXTERN_BLOCK_SIZE;
  extern_ptr = extern_block;
  extern_block_malloced = 1;
}

static void resize_extern_block(int required)
{
  long curr_pos, size, reqd_size;

  if (! extern_block_malloced) {
    initial_ofs += obj_counter;
    free_extern_table();
    failwith("Marshal.to_buffer: buffer overflow");
  }
  curr_pos = extern_ptr - extern_block;
  size = extern_limit - extern_block;
  reqd_size = curr_pos + required;
  while (size <= reqd_size) size *= 2;
  extern_block = stat_resize(extern_block, size);
  extern_limit = extern_block + size;
  extern_ptr = extern_block + curr_pos;
}

/* Write characters, integers, and blocks in the output buffer */

#define Write(c) \
  if (extern_ptr >= extern_limit) resize_extern_block(1); \
  *extern_ptr++ = (c)

static void writeblock(char *data, long int len)
{
  if (extern_ptr + len > extern_limit) resize_extern_block(len);
  bcopy(data, extern_ptr, len);
  extern_ptr += len;
}

static void writecode8(int code, long int val)
{
  if (extern_ptr + 2 > extern_limit) resize_extern_block(2);
  extern_ptr[0] = code;
  extern_ptr[1] = val;
  extern_ptr += 2;
}

static void writecode16(int code, long int val)
{
  if (extern_ptr + 3 > extern_limit) resize_extern_block(3);
  extern_ptr[0] = code;
  extern_ptr[1] = val >> 8;
  extern_ptr[2] = val;
  extern_ptr += 3;
}

static void write32(long int val)
{
  if (extern_ptr + 4 > extern_limit) resize_extern_block(4);
  extern_ptr[0] = val >> 24;
  extern_ptr[1] = val >> 16;
  extern_ptr[2] = val >> 8;
  extern_ptr[3] = val;
  extern_ptr += 4;
}

static void writecode32(int code, long int val)
{
  if (extern_ptr + 5 > extern_limit) resize_extern_block(5);
  extern_ptr[0] = code;
  extern_ptr[1] = val >> 24;
  extern_ptr[2] = val >> 16;
  extern_ptr[3] = val >> 8;
  extern_ptr[4] = val;
  extern_ptr += 5;
}

#ifdef ARCH_SIXTYFOUR
static void writecode64(int code, long val)
{
  int i;
  if (extern_ptr + 9 > extern_limit) resize_extern_block(9);
  *extern_ptr ++ = code;
  for (i = 64 - 8; i >= 0; i -= 8) *extern_ptr++ = val >> i;
}
#endif

/* Marshal the given value in the output buffer */

static unsigned long size_32;  /* Size in words of 32-bit block for struct. */
static unsigned long size_64;  /* Size in words of 64-bit block for struct. */

static int extern_ignore_sharing; /* Flag to ignore sharing */
static int extern_closures;     /* Flag to allow externing code pointers */

static void extern_invalid_argument(char *msg)
{
  if (extern_block_malloced) stat_free(extern_block);
  initial_ofs += obj_counter;
  free_extern_table();
  invalid_argument(msg);
}

static int closure_level;

static void extern_rec(value v)
{
 tailcall:
  if (Is_long(v)) {
    long n = Long_val(v);
    if (n >= 0 && n < 0x40) {
      Write(PREFIX_SMALL_INT + n);
    } else if (n >= -(1 << 7) && n < (1 << 7)) {
      writecode8(CODE_INT8, n);
    } else if (n >= -(1 << 15) && n < (1 << 15)) {
      writecode16(CODE_INT16, n);
#ifdef ARCH_SIXTYFOUR
    } else if (n < -(1L << 31) || n >= (1L << 31)) {
      writecode64(CODE_INT64, n);
#endif
    } else
      writecode32(CODE_INT32, n);
    return;
  }
  if (Is_young(v) || Is_in_heap(v) || Is_atom(v)) {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);
    asize_t h;
    /* Atoms are treated specially for two reasons: they are not allocated
       in the externed block, and they are automatically shared. */
    if (sz == 0) {
      if (tag < 16) {
        Write(PREFIX_SMALL_BLOCK + tag);
      } else {
        writecode32(CODE_BLOCK32, hd);
      }
      return;
    }
    /* Check if already seen */
    if (! extern_ignore_sharing) {
      if (2 * obj_counter >= extern_table_size) resize_extern_table();
      h = Hash(v);
      while (extern_table[h].ofs >= initial_ofs) {
        if (extern_table[h].obj == v) {
          byteoffset_t d = obj_counter - (extern_table[h].ofs - initial_ofs);
          if (d < 0x100) {
            writecode8(CODE_SHARED8, d);
          } else if (d < 0x10000) {
            writecode16(CODE_SHARED16, d);
          } else {
            writecode32(CODE_SHARED32, d);
          }
          return;
        }
        h++;
        if (h >= extern_table_size) h = 0;
      }
      /* Not seen yet. Record the object */
      extern_table[h].ofs = initial_ofs + obj_counter;
      extern_table[h].obj = v;
      obj_counter++;
    }
    /* Output the contents of the object */
    switch(tag) {
    case String_tag: {
      mlsize_t len = string_length(v);
      if (len < 0x20) {
        Write(PREFIX_SMALL_STRING + len);
      } else if (len < 0x100) {
        writecode8(CODE_STRING8, len);
      } else {
        writecode32(CODE_STRING32, len);
      }
      writeblock(String_val(v), len);
      size_32 += 1 + (len + 4) / 4;
      size_64 += 1 + (len + 8) / 8;
      break;
    }
    case Double_tag: {
      if (sizeof(double) != 8)
        extern_invalid_argument("output_value: non-standard floats");
      Write(CODE_DOUBLE_NATIVE);
      writeblock((char *) v, 8);
      size_32 += 1 + 2;
      size_64 += 1 + 1;
      break;
    }
    case Double_array_tag: {
      mlsize_t nfloats;
      if (sizeof(double) != 8)
        extern_invalid_argument("output_value: non-standard floats");
      nfloats = Wosize_val(v) / Double_wosize;
      if (nfloats < 0x100) {
        writecode8(CODE_DOUBLE_ARRAY8_NATIVE, nfloats);
      } else {
        writecode32(CODE_DOUBLE_ARRAY32_NATIVE, nfloats);
      }
      writeblock((char *) v, Bosize_val(v));
      size_32 += 1 + nfloats * 2;
      size_64 += 1 + nfloats;
      break;
    }
    case Abstract_tag:
    case Final_tag:
      extern_invalid_argument("output_value: abstract value");
      break;
    case Infix_tag:
      writecode32(CODE_INFIXPOINTER, Infix_offset_hd(hd));
      extern_rec(v - Infix_offset_hd(hd));
      break;
    case Object_tag:
      extern_invalid_argument("output_value: object value");
      break;
    case Closure_tag: {
      mlsize_t i;
      if (tag < 16 && sz < 8) {
        Write(PREFIX_SMALL_BLOCK + tag + (sz << 4));
      } else {
        writecode32(CODE_BLOCK32, hd & ~Black);
      }
      size_32 += 1 + sz;
      size_64 += 1 + sz;
      closure_level++;
      for (i = 0; i < sz; i++) extern_rec(Field(v, i));
      closure_level--;
      return;
      }
    default: {
      mlsize_t i;
      if (tag < 16 && sz < 8) {
        Write(PREFIX_SMALL_BLOCK + tag + (sz << 4));
      } else {
        writecode32(CODE_BLOCK32, hd & ~Black);
      }
      size_32 += 1 + sz;
      size_64 += 1 + sz;
      for (i = 0; i < sz - 1; i++) extern_rec(Field(v, i));
      v = Field(v, i);
      goto tailcall;
      }
    }
    return;
  }
  if ((char *) v >= code_area_start && (char *) v < code_area_end) {
    int i;
    if (!extern_closures)
      extern_invalid_argument("output_value: functional value");
    if(function_symbols) {
        for(i = 0; i != closure_level; i++)
            putc(' ', stderr);
        print_symbol((char *) v - code_area_start);
    }
    writecode32(CODE_CODEPOINTER, (char *) v - code_area_start);
    writeblock((char *) code_checksum(), 16);
    return;
  }
#if 0
  else {
    int i;
    fprintf(stderr, "Output C function\n");
    if (!extern_closures)
      extern_invalid_argument("output_value: functional value");
    if(function_symbols) {
        for(i = 0; i != closure_level; i++)
            putc(' ', stderr);
        print_symbol((char *) v - code_area_start);
    }
    writecode32(CODE_CODEPOINTER2, (unsigned long) v);
    writeblock((char *) code_checksum(), 16);
    return;
  }
#endif
  extern_invalid_argument("output_value: abstract value");
}

enum { NO_SHARING = 1, CLOSURES = 2 };
static int extern_flags[] = { NO_SHARING, CLOSURES };

static long extern_value(value v, value flags)
{
  long res_len;
  int fl;
  /* Parse flag list */
  fl = convert_flag_list(flags, extern_flags);
  extern_ignore_sharing = fl & NO_SHARING;
  extern_closures = fl & CLOSURES;
  /* Allocate hashtable of objects already seen, if needed */
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  if (extern_table == NULL) {
    alloc_extern_table();
    initial_ofs = 1;
  }
  obj_counter = 0;
  size_32 = 0;
  size_64 = 0;
  /* Write magic number */
  write32(Intext_magic_number);
  /* Set aside space for the sizes */
  extern_ptr += 4*4;
  /* Marshal the object */
  extern_rec(v);
  /* Update initial offset for next call to extern_value(),
     if we decide to keep the table of shared objects. */
  initial_ofs += obj_counter;
  /* Free the table of shared objects (if needed) */
  free_extern_table();
  /* Write the sizes */
#ifdef ARCH_SIXTYFOUR
  if (size_32 >= (1L << 32) || size_64 >= (1L << 32)) {
    /* The object is so big its size cannot be written in the header.
       Besides, some of the block sizes or string lengths or shared offsets
       it contains may have overflowed the 32 bits used to write them. */
    failwith("output_value: object too big");
  }
#endif
  res_len = extern_ptr - extern_block;
  extern_ptr = extern_block + 4;
  write32(res_len - 5*4);
  write32(obj_counter);
  write32(size_32);
  write32(size_64);
  /* Result is res_len bytes starting at extern_block */
  return res_len;
}

void output_val(struct channel *chan, value v, value flags)
{
  long len;
  if (! channel_binary_mode(chan))
    failwith("output_value: not a binary channel");
  alloc_extern_block();
  len = extern_value(v, flags);
  really_putblock(chan, extern_block, len);
  stat_free(extern_block);
}

value output_value(value vchan, value v, value flags) /* ML */
{
  struct channel * channel = Channel(vchan);
  Begin_roots2(v, flags)
    Lock(channel);
    output_val(channel, v, flags);
    Unlock(channel);
  End_roots();
  return Val_unit;
}

value output_value_to_string(value v, value flags) /* ML */
{
  long len;
  value res;
  alloc_extern_block();
  len = extern_value(v, flags);
  res = alloc_string(len);
  bcopy(extern_block, String_val(res), len);
  stat_free(extern_block);
  return res;
}

value output_value_to_buffer(value buf, value ofs, value len, value v, value flags) /* ML */
{
  long len_res;
  extern_block = &Byte(buf, Long_val(ofs));
  extern_limit = extern_block + Long_val(len);
  extern_ptr = extern_block;
  extern_block_malloced = 0;
  len_res = extern_value(v, flags);
  return Val_long(len_res);
}

void output_value_to_malloc(value v, value flags,
                            /*out*/ char ** buf, /*out*/ long * len)
{
  alloc_extern_block();
  *buf = extern_block;
  *len = extern_value(v, flags);
}

