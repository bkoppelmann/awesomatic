
#ifndef GRAMMAR_H
#define GRAMMAR_H
#include "deftbl.h"
#include "envmod.h"

#include "DefTableKeyList.h"

extern DefTableKey GrammarRoot;
extern int MultipleRoots;
extern DefTableKeyList TreeSymbols;

extern void TransformListofRules (Environment env);

extern void ClassifySymbols ();
#endif
