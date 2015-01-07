/* rbtree.c                                               -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: $
 */
#define LIBSAGITTARIUS_BODY
#include "sagittarius/treemap.h"
#include "sagittarius/error.h"
#include "sagittarius/vm.h"

/* 
   Based on Java's implementation
 */
#define BLACK ((char)0)
#define RED   ((char)1)

typedef struct node_rec_t
{
  intptr_t key;
  intptr_t value;
  char     color;
  struct node_rec_t *parent;
  struct node_rec_t *left;
  struct node_rec_t *right;
} node_t;

static node_t* new_node(intptr_t key, node_t *parent)
{
  node_t *n = SG_NEW(node_t);
  n->key = key;
  n->parent = parent;
  n->left = NULL;
  n->right = NULL;
  n->color = BLACK;
  return n;
}

#define NODE(n) 	((node_t*)n)
#if 0
#define SET_COLOR(n, c) if (n) {NODE(n)->color = (c);}
#define PARENT_OF(p)    ((p) ? NODE(p)->parent : NULL)
#define LEFT_OF(p)      ((p) ? NODE(p)->left : NULL)
#define RIGHT_OF(p)     ((p) ? NODE(p)->right : NULL)
#define COLOR_OF(p)     ((p) ? NODE(p)->color : BLACK)
#else
static void set_color(node_t *n, char color)
{
  if (n) n->color = color;
}
static node_t* parent_of(node_t *p) 
{
  return (p) ? NODE(p)->parent : NULL;
}
static node_t* left_of(node_t *p) 
{
  return (p) ? NODE(p)->left : NULL;
}
static node_t* right_of(node_t *p) 
{
  return (p) ? NODE(p)->right : NULL;
}
static char color_of(node_t *p) 
{
  return (p) ? NODE(p)->color : BLACK;
}

#define SET_COLOR(n, c) set_color(NODE(n), c)
#define PARENT_OF(p)    parent_of(NODE(p))
#define LEFT_OF(p)      left_of(NODE(p))
#define RIGHT_OF(p)     right_of(NODE(p))
#define COLOR_OF(p)     color_of(NODE(p))
#endif

static node_t* successor(node_t *t)
{
  if (t == NULL) return NULL;
  else if (t->right != NULL) {
    node_t *p = t->right;
    while (p->left != NULL) p = p->left;
    return p;
  } else {
    node_t *p = t->parent;
    node_t *ch = t;
    while (p != NULL && ch == p->right) {
      ch = p;
      p = p->parent;
    }
    return p;
  }
}

static void rotate_left(SgTreeMap *tm, node_t *p)
{
  if (p != NULL) {
    node_t *r = p->right;
    p->right = r->left;
    if (r->left != NULL) r->left->parent = p;
    r->parent = p->parent;
    if (p->parent == NULL) tm->root = (intptr_t)r;
    else if (p->parent->left == p) p->parent->left = r;
    else p->parent->right = r;
    r->left = p;
    p->parent = r;
  }
}

static void rotate_right(SgTreeMap *tm, node_t *p)
{
  if (p != NULL) {
    node_t *l = p->left;
    p->left = l->right;
    if (l->right != NULL) l->right->parent = p;
    l->parent = p->parent;
    if (p->parent == NULL) tm->root = (intptr_t)l;
    else if (p->parent->right == p) p->parent->right = l;
    else p->parent->left = l;
    l->right = p;
    p->parent = l;
  }
}

static void fix_after_insertion(SgTreeMap *tm, node_t *x)
{
  x->color = RED;
  while (x != NULL && NODE(tm->root) != x && x->parent->color == RED) {
    if (PARENT_OF(x) == LEFT_OF(PARENT_OF(PARENT_OF(x)))) {
      node_t *y = RIGHT_OF(PARENT_OF(PARENT_OF(x)));
      if (COLOR_OF(y) == RED) {
	SET_COLOR(PARENT_OF(x), BLACK);
	SET_COLOR(y, BLACK);
	SET_COLOR(PARENT_OF(PARENT_OF(x)), RED);
	x = PARENT_OF(PARENT_OF(x));
      } else {
	if (x == RIGHT_OF(PARENT_OF(x))) {
	  x = PARENT_OF(x);
	  rotate_left(tm, x);
	}
	SET_COLOR(PARENT_OF(x), BLACK);
	SET_COLOR(PARENT_OF(PARENT_OF(x)), RED);
	rotate_right(tm, PARENT_OF(PARENT_OF(x)));
      }
    } else {
      node_t *y = LEFT_OF(PARENT_OF(PARENT_OF(x)));
      if (COLOR_OF(y) == RED) {
	SET_COLOR(PARENT_OF(x), BLACK);
	SET_COLOR(y, BLACK);
	SET_COLOR(PARENT_OF(PARENT_OF(x)), RED);
	x = PARENT_OF(PARENT_OF(x));
      } else {
	if (x == LEFT_OF(PARENT_OF(x))) {
	  x = PARENT_OF(x);
	  rotate_right(tm, x);
	}
	SET_COLOR(PARENT_OF(x), BLACK);
	SET_COLOR(PARENT_OF(PARENT_OF(x)), RED);
	rotate_left(tm, PARENT_OF(PARENT_OF(x)));
      }
    }
  }
  NODE(tm->root)->color = BLACK;
}

static node_t* get_entry(SgTreeMap *tm, intptr_t key)
{
  intptr_t k = key;
  SgTreeCompareProc *cpr = SG_TREEMAP_C_PROC(tm, cmp);
  node_t *p = (node_t*)tm->root;
  while (p != NULL) {
    int cmp = cpr(tm, k, p->key);
    if (cmp < 0) {
      p = p->left;
    } else if (cmp > 0) {
      p = p->right;
    } else {
      return p;
    }
  }
  return NULL;
}

static SgTreeEntry* rb_ref(SgTreeMap *tm, intptr_t key)
{
  return (SgTreeEntry*)get_entry(tm, key);
}

static node_t* get_higher_entry(SgTreeMap *tm, intptr_t key)
{
  node_t *p = NODE(tm->root);
  while (p != NULL) {
    int cmp = SG_TREEMAP_C_PROC(tm, cmp)(tm, key, p->key);
    if (cmp < 0) {
      if (p->left != NULL) p = p->left;
      else return p;
    } else {
      if (p->right != NULL) p = p->right;
      else {
	node_t *parent = p->parent;
	node_t *ch = p;
	while (parent != NULL && ch == parent->right) {
	  ch = parent;
	  parent = parent->parent;
	}
	return parent;
      }
    }
  }
  return NULL;
}

static SgTreeEntry* rb_higher(SgTreeMap *tm, intptr_t key)
{
  return (SgTreeEntry*)get_higher_entry(tm, key);
}

static node_t* get_lower_entry(SgTreeMap *tm, intptr_t key)
{
  node_t *p = NODE(tm->root);
  while (p != NULL) {
    int cmp = SG_TREEMAP_C_PROC(tm, cmp)(tm, key, p->key);
    if (cmp > 0) {
      if (p->right != NULL) p = p->right;
      else return p;
    } else {
      if (p->left != NULL) p = p->left;
      else {
	node_t *parent = p->parent;
	node_t *ch = p;
	while (parent != NULL && ch == parent->left) {
	  ch = parent;
	  parent = parent->parent;
	}
	return parent;
      }
    }
  }
  return NULL;
}

static SgTreeEntry* rb_lower(SgTreeMap *tm, intptr_t key)
{
  return (SgTreeEntry*)get_lower_entry(tm, key);
}

/* only creates an entry and return it */
static SgTreeEntry* rb_set(SgTreeMap *tm, intptr_t key)
{
  node_t *t = (node_t*)tm->root;
  if (t == NULL) {
    node_t *nn = new_node(key, NULL);
    tm->root = (intptr_t)nn;
    tm->entryCount++;
    return (SgTreeEntry*)nn;
  } else {
    int cmp;
    node_t *parent, *e;
    SgTreeCompareProc *cpr = SG_TREEMAP_C_PROC(tm, cmp);
    do {
      parent = t;
      cmp = cpr(tm, (intptr_t)key, t->key);
      if (cmp < 0) {
	t = t->left;
      } else if (cmp > 0) {
	t = t->right;
      } else {
	return (SgTreeEntry*)t;
      }
    } while (t != NULL);
    e = new_node(key, parent);
    if (cmp < 0) {
      parent->left = e;
    } else {
      parent->right = e;
    }
    /* fprintf(stderr, "cmp %d, key: %ld\n", cmp, SG_INT_VALUE(key)); */
    fix_after_insertion(tm, e);
    tm->entryCount++;
    return (SgTreeEntry*)e;
  }
}

static void fix_after_deletion(SgTreeMap *tm, node_t *x)
{
  while (x != NODE(tm->root) && COLOR_OF(x) == BLACK) {
    if (x == LEFT_OF(PARENT_OF(x))) {
      node_t *sib = RIGHT_OF(PARENT_OF(x));
      if (COLOR_OF(sib) == RED) {
	SET_COLOR(sib, BLACK);
	SET_COLOR(PARENT_OF(x), RED);
	rotate_left(tm, PARENT_OF(x));
	sib = RIGHT_OF(PARENT_OF(x));
      }

      if (COLOR_OF(LEFT_OF(sib))  == BLACK &&
	  COLOR_OF(RIGHT_OF(sib)) == BLACK) {
	x = PARENT_OF(x);
      } else {
	if (COLOR_OF(RIGHT_OF(sib)) == BLACK) {
	  SET_COLOR(LEFT_OF(sib), BLACK);
	  SET_COLOR(sib, RED);
	  rotate_right(tm, sib);
	  sib = RIGHT_OF(PARENT_OF(x));
	}
	SET_COLOR(sib, COLOR_OF(PARENT_OF(x)));
	SET_COLOR(PARENT_OF(x), BLACK);
	SET_COLOR(RIGHT_OF(sib), BLACK);
	rotate_left(tm, PARENT_OF(x));
	x = (node_t*)tm->root;
      }
    } else {
      node_t *sib = LEFT_OF(PARENT_OF(x));
      if (COLOR_OF(sib) == RED) {
	SET_COLOR(sib, BLACK);
	SET_COLOR(PARENT_OF(x), RED);
	rotate_right(tm, PARENT_OF(x));
	sib = LEFT_OF(PARENT_OF(x));
      }
      if (COLOR_OF(RIGHT_OF(sib)) == BLACK &&
	  COLOR_OF(LEFT_OF(sib))  == BLACK) {
	SET_COLOR(sib, RED);
	x = PARENT_OF(x);
      } else {
	if (COLOR_OF(LEFT_OF(sib)) == BLACK) {
	  SET_COLOR(RIGHT_OF(sib), BLACK);
	  SET_COLOR(sib, RED);
	  rotate_left(tm, sib);
	  sib = LEFT_OF(PARENT_OF(x));
	}
	SET_COLOR(sib, COLOR_OF(PARENT_OF(x)));
	SET_COLOR(PARENT_OF(x), BLACK);
	SET_COLOR(LEFT_OF(sib), BLACK);
	rotate_right(tm, PARENT_OF(x));
	x = (node_t*)tm->root;
      }
    }
  }
}

static void delete_entry(SgTreeMap *tm, node_t *p)
{
  node_t *replacement;
  tm->entryCount--;
  if (p->left != NULL && p->right != NULL) {
    node_t *s = successor(p);
    p->key = s->key;
    p->value = s->value;
    p = s;
  }
  replacement = (p->left) ? p->left : p->right;
  if (replacement != NULL) {
    replacement->parent = p->parent;
    if (p->parent == NULL) tm->root = (intptr_t)replacement;
    else if (p == p->parent->left) p->parent->left = replacement;
    else p->parent->right = replacement;
    p->left = p->right = p->parent = NULL;

    if (p->color == BLACK) {
      fix_after_deletion(tm, replacement);
    }
  } else if (p->parent == NULL) {
    tm->root = (intptr_t)NULL;
  } else {
    if (p->color == BLACK) {
      fix_after_deletion(tm, p);
    }
    if (p->parent != NULL) {
      if (p == p->parent->left) p->parent->left = NULL;
      else if (p == p->parent->right) p->parent->right = NULL;
      p->parent = NULL;
    }
  }
}

static SgTreeEntry* rb_delete(SgTreeMap *tm, intptr_t key)
{
  node_t *p = get_entry(tm, key);
  if (p == NULL) return NULL;
  delete_entry(tm, p);
  return (SgTreeEntry *)p;
}

static node_t* copy_tree(node_t *parent, node_t *self)
{
  node_t *n = new_node(self->key, parent);
  n->value = self->value;
  n->color = self->color;
  if (self->left) n->left = copy_tree(n, self->left);
  if (self->right) n->right = copy_tree(n, self->right);
  return n;
}

static SgObject rb_copy(const SgTreeMap *tm);
static SgTreeIter* rb_iter(SgTreeIter *iter, SgTreeMap *tm,
			   SgTreeEntry *start);

static SgTreeEntry* rb_search(SgTreeMap *tm, intptr_t key, SgDictOp op)
{
  switch (op) {
  case SG_DICT_GET: return rb_ref(tm, key);
  case SG_DICT_CREATE: return rb_set(tm, key);
  case SG_DICT_DELETE: return rb_delete(tm, key);
  }
  Sg_Error(UC("[Internal] Operation is not supported."));
  return NULL;
} 

static SgObject rb_copy(const SgTreeMap *tm)
{
  SgTreeMap *dst = Sg_MakeGenericCTreeMap(SG_TREEMAP_C_PROC(tm, cmp),
					  rb_search,
					  rb_copy,
					  rb_iter,
					  rb_higher,
					  rb_lower,
					  tm->data);
  if (tm->root) {
    dst->root = (intptr_t)copy_tree(NULL, NODE(tm->root));
  } else {
    dst->root = (intptr_t)NULL;
  }
  dst->entryCount = tm->entryCount;
  return dst;
}

static node_t* get_first_entry(SgTreeMap *tm)
{
  node_t *p = NODE(tm->root);
  if (p != NULL)
    while (p->left != NULL) p = p->left;
  return p;
}

static SgTreeEntry *rb_iter_next(SgTreeIter *iter)
{
  if (iter->end) return NULL;
  if (iter->e) {
    iter->e = (SgTreeEntry*)successor((node_t*)iter->e);
  } else {
    iter->e = (SgTreeEntry*)get_first_entry(iter->t);
  }
  if (iter->e == NULL) iter->end = TRUE;
  return (SgTreeEntry*)iter->e;
}

static SgTreeIter* rb_iter(SgTreeIter *iter, SgTreeMap *tm,
			   SgTreeEntry *start)
{
  if (start && get_entry(tm, start->key) != (node_t*)start) {
    Sg_Error(UC("rb_iter: iteration start point is not a part of the tree."));
  }
  iter->next = rb_iter_next;
  iter->t = tm;
  iter->e = start;
  iter->end = FALSE;
  return iter;
}

SgObject Sg_MakeRBTreeMap(SgTreeCompareProc *cmp)
{
  return Sg_MakeGenericCTreeMap(cmp,
				rb_search,
				rb_copy,
				rb_iter,
				rb_higher,
				rb_lower,
				NULL);
}

static int wrapped_compare(SgTreeMap *tm, intptr_t a, intptr_t b)
{
  SgObject r;
  if (SG_SUBRP(tm->data)) {
    SG_CALL_SUBR2(r, tm->data, SG_OBJ(a), SG_OBJ(b));
  } else {
    r = Sg_Apply2(SG_OBJ(tm->data), SG_OBJ(a), SG_OBJ(b));
  }
  if (SG_INTP(r)) return SG_INT_VALUE(r);
  Sg_Error(UC("compare returned non exact integer value %S"), r);
  return 0; 			/* dummy */
}

SgObject Sg_MakeSchemeRBTreeMap(SgObject cmp)
{
  return Sg_MakeGenericCTreeMap(wrapped_compare,
				rb_search,
				rb_copy,
				rb_iter,
				rb_higher,
				rb_lower,
				cmp);
}
