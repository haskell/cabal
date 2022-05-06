/*
 * This code is taken from the RPM package manager.
 *
 * RPM is Copyright (c) 1998 by Red Hat Software, Inc.,
 * and may be distributed under the terms of the GPL and LGPL.
 * See http://rpm.org/gitweb?p=rpm.git;a=blob_plain;f=COPYING;hb=HEAD
 *
 * The code should follow upstream as closely as possible.
 * See http://rpm.org/gitweb?p=rpm.git;a=blob_plain;f=lib/rpmvercmp.c;hb=HEAD
 *
 * Currently the only difference as a policy is that upstream uses C99
 * features and pkg-config does not require a C99 compiler yet.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/* macros to help code look more like upstream */
#define rstreq(a, b)	(strcmp(a, b) == 0)
#define risalnum(c)	isalnum((char)(c))
#define risdigit(c)	isdigit((char)(c))
#define risalpha(c)	isalpha((char)(c))

int rmpvercmp_impl(const char *a, const char *b, char *buf1, char *buf2);

/* compare alpha and numeric segments of two versions */
/* return 1: a is newer than b */
/*        0: a and b are the same version */
/*       -1: b is newer than a */
int rpmvercmp(const char * a, const char * b)
{
    /* easy comparison to see if versions are identical */
    if (rstreq(a, b)) return 0;

    char *buf1 = malloc(strlen(a) + 1);
    char *buf2 = malloc(strlen(b) + 1);
    
    if (!buf1 || !buf2) return 0; // arbitrary

    int r = rmpvercmp_impl(a, b, buf1, buf2);

    free(buf1);
    free(buf2);

    return r;
}

int rmpvercmp_impl(const char *a, const char *b, char *str1, char *str2) {
    char oldch1, oldch2;
    char * one, * two;
    int rc;
    int isnum;

    strcpy(str1, a);
    strcpy(str2, b);

    one = str1;
    two = str2;

    /* loop through each version segment of str1 and str2 and compare them */
    while (*one && *two) {
	while (*one && !risalnum(*one)) one++;
	while (*two && !risalnum(*two)) two++;

	/* If we ran to the end of either, we are finished with the loop */
	if (!(*one && *two)) break;

	str1 = one;
	str2 = two;

	/* grab first completely alpha or completely numeric segment */
	/* leave one and two pointing to the start of the alpha or numeric */
	/* segment and walk str1 and str2 to end of segment */
	if (risdigit(*str1)) {
	    while (*str1 && risdigit(*str1)) str1++;
	    while (*str2 && risdigit(*str2)) str2++;
	    isnum = 1;
	} else {
	    while (*str1 && risalpha(*str1)) str1++;
	    while (*str2 && risalpha(*str2)) str2++;
	    isnum = 0;
	}

	/* save character at the end of the alpha or numeric segment */
	/* so that they can be restored after the comparison */
	oldch1 = *str1;
	*str1 = '\0';
	oldch2 = *str2;
	*str2 = '\0';

	/* this cannot happen, as we previously tested to make sure that */
	/* the first string has a non-null segment */
	if (one == str1) return -1;	/* arbitrary */

	/* take care of the case where the two version segments are */
	/* different types: one numeric, the other alpha (i.e. empty) */
	/* numeric segments are always newer than alpha segments */
	/* XXX See patch #60884 (and details) from bugzilla #50977. */
	if (two == str2) return (isnum ? 1 : -1);

	if (isnum) {
	    /* this used to be done by converting the digit segments */
	    /* to ints using atoi() - it's changed because long  */
	    /* digit segments can overflow an int - this should fix that. */

	    /* throw away any leading zeros - it's a number, right? */
	    while (*one == '0') one++;
	    while (*two == '0') two++;

	    /* whichever number has more digits wins */
	    if (strlen(one) > strlen(two)) return 1;
	    if (strlen(two) > strlen(one)) return -1;
	}

	/* strcmp will return which one is greater - even if the two */
	/* segments are alpha or if they are numeric.  don't return  */
	/* if they are equal because there might be more segments to */
	/* compare */
	rc = strcmp(one, two);
	if (rc) return (rc < 1 ? -1 : 1);

	/* restore character that was replaced by null above */
	*str1 = oldch1;
	one = str1;
	*str2 = oldch2;
	two = str2;
    }

    /* this catches the case where all numeric and alpha segments have */
    /* compared identically but the segment separating characters were */
    /* different */
    if ((!*one) && (!*two)) return 0;

    /* whichever version still has characters left over wins */
    if (!*one) return -1; else return 1;
}
