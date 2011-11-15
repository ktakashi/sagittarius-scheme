/* rbtree.c                                               -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2011  Takashi Kato <ktakashi@ymail.com>
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
#define SET_COLOR(n, c) if (n) {NODE(n)->color = (c);}
#define PARENT_OF(p)    ((p) ? NODE(p)->parent : NULL)
#define LEFT_OF(p)      ((p) ? NODE(p)->left : NULL)
#define RIGHT_OF(p)     ((p) ? NODE(p)->right : NULL)
#define COLOR_OF(p)     ((p) ? NODE(p)->color : BLACK)

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
    if (r->left != NULL) p->left->parent = p;
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
    p->right = l->right;
    if (l->right != NULL) p->right->parent = p;
    l->parent = p->parent;
    if (p->parent == NULL) tm->root = (intptr_t)l;
    else if (p->parent->right == p) p->parent->left = l;
    else p->parent->left = l;
    l->right = p;
    p->parent = l;
  }
}

static void fix_after_insertion(SgTreeMap *tm, node_t *x)
{
  SET_COLOR(x, RED);
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
  SET_COLOR(tm->root, BLACK);
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

static SgTreeEntry* rb_ref(SgTreeMap *tm, SgObject key)
{
  return (SgTreeEntry*)get_entry(tm, (intptr_t)key);
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

static SgTreeEntry* rb_higher(SgTreeMap *tm, SgObject key)
{
  return (SgTreeEntry*)get_higher_entry(tm, (intptr_t)key);
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

static SgTreeEntry* rb_lower(SgTreeMap *tm, SgObject key)
{
  return (SgTreeEntry*)get_lower_entry(tm, (intptr_t)key);
}

static SgTreeEntry* rb_set(SgTreeMap *tm, SgObject key, SgObject value, int flags)
{
  node_t *t = (node_t*)tm->root;
  if (t == NULL) {
    if (flags & SG_TREE_NO_CREATE) {
      return SG_UNBOUND;
    } else {
      node_t *nn = new_node((intptr_t)key, NULL);
      nn->value = (intptr_t)value;
      tm->root = (intptr_t)nn;
      tm->entryCount++;
      return (SgTreeEntry*)nn;
    }
  } else {
    int cmp;
    node_t *parent;
    SgTreeCompareProc *cpr = SG_TREEMAP_C_PROC(tm, cmp);
    do {
      parent = t;
      cmp = cpr(tm, (intptr_t)key, t->key);
      if (cmp < 0) {
	t = t->left;
      } else if (cmp > 0) {
	t = t->right;
      } else {
	if (flags & SG_TREE_NO_OVERWRITE) {
	  return (SgTreeEntry*)t;
	} else {
	  t->value = (intptr_t)value;
	  return (SgTreeEntry*)t;
	}
      }
    } while (t != NULL);
    if (flags & SG_TREE_NO_CREATE) {
      return SG_UNBOUND;
    } else {
      node_t *e = new_node((intptr_t)key, parent);
      e->value = (intptr_t)value;
      if (cmp < 0) {
	parent->left = e;
      } else {
	parent->right = e;
      }
      fix_after_insertion(tm, e);
      tm->entryCount++;
      return (SgTreeEntry*)e;
    }
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

static SgObject rb_delete(SgTreeMap *tm, SgObject key)
{
  node_t *p = get_entry(tm, (intptr_t)key);
  intptr_t old;
  if (p == NULL) return SG_UNBOUND;
  old = p->value;
  delete_entry(tm, p);
  return SG_OBJ(old);
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

static SgObject rb_copy(const SgTreeMap *tm)
{
  SgTreeMap *dst = Sg_MakeGenericCTreeMap(SG_TREEMAP_C_PROC(tm, cmp),
					  rb_ref,
					  rb_set,
					  rb_delete,
					  rb_copy,
					  rb_iter,
					  rb_higher,
					  rb_lower);
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
				rb_ref,
				rb_set,
				rb_delete,
				rb_copy,
				rb_iter,
				rb_higher,
				rb_lower);
}

