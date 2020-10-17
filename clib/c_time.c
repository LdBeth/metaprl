/*
 * Date functions.
 */
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <memory.h>

#ifndef WIN32
#include <unistd.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

/*
 * Get the date.
 */
value c_localtime(value v_date)
{
    CAMLparam1(v_date);
    CAMLlocal1(tuple);
    struct tm *tmp;
    time_t when;

    when = Int_val(v_date);
    tmp = localtime(&when);
    tuple = alloc_tuple(7);
    Field(tuple, 0) = Val_int(tmp->tm_sec);
    Field(tuple, 1) = Val_int(tmp->tm_min);
    Field(tuple, 2) = Val_int(tmp->tm_hour);
    Field(tuple, 3) = Val_int(tmp->tm_mday);
    Field(tuple, 4) = Val_int(tmp->tm_mon + 1);
    Field(tuple, 5) = Val_int(tmp->tm_year + 1900);
    Field(tuple, 6) = Val_int(0);
    CAMLreturn(tuple);
}

/*
 * Parse the date.
 */
value c_mktime(value v_date)
{
    CAMLparam1(v_date);
    struct tm time;
    time_t when;

    memset(&time, 0, sizeof(time));
    time.tm_sec = Int_val(Field(v_date, 0));
    time.tm_min = Int_val(Field(v_date, 1));
    time.tm_hour = Int_val(Field(v_date, 2));
    time.tm_mday = Int_val(Field(v_date, 3));
    time.tm_mon = Int_val(Field(v_date, 4)) - 1;
    time.tm_year = Int_val(Field(v_date, 5)) - 1900;
    when = mktime(&time);
    when += Int_val(Field(v_date, 6));
    CAMLreturn(Val_int(when));
}

/*
 * Convert the date.
 */
value c_simpletime(value v_date)
{
    CAMLparam1(v_date);
    CAMLlocal1(v_string);
    char buffer[100];
    struct tm *fmt;
    time_t date;

    date = Int_val(v_date);
    fmt = localtime(&date);
    sprintf(buffer, "%d/%d/%02d", fmt->tm_mon + 1, fmt->tm_mday, fmt->tm_year % 100);
    v_string = copy_string(buffer);
    CAMLreturn(v_string);
}

value c_ctime(value v_date)
{
    CAMLparam1(v_date);
    CAMLlocal1(v_string);
    char buffer[100];
    struct tm *tmp;
    time_t date;

    date = Int_val(v_date);
    strcpy(buffer, ctime(&date));
    buffer[strlen(buffer) - 1] = 0;
    v_string = copy_string(buffer);
    CAMLreturn(v_string);
}
