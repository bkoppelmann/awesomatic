/* This file is part of the Eli translator construction system.

Eli is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

Eli is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with Eli; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
/* OPTIM.h - IDL declarations generated by idlc, version 3.2 on Thu Jan 10
 * 16:37:19 1991 */
/* * modified by HaDeS: 'IDLtag' replaced by 'IDLtag_OPTIM' to prevent
 * conflicts * with corresponding type definitions in LIGA.h */

#include "global.h"

/* Private Types */

/* Class Headers */
typedef struct CAAction *CPAction;

/* Nodes */
typedef struct RAttributes *Attributes;
#define KAttributes 2
#define NAttributes (IAttributes((Attributes)N_INIT( \
	GetNode(sizeof(struct RAttributes),KAttributes), \
	KAttributes,sizeof(struct RAttributes))))
#define DAttributes(n) {FAttributes(n); FreeNode(n, KAttributes);}
#define IAttributes(N) (N)
#define FAttributes(N)

typedef struct RCan_Eval *Can_Eval;
#define KCan_Eval 4
#define NCan_Eval (ICan_Eval((Can_Eval)N_INIT( \
	GetNode(sizeof(struct RCan_Eval),KCan_Eval), \
	KCan_Eval,sizeof(struct RCan_Eval))))
#define DCan_Eval(n) {FCan_Eval(n); FreeNode(n, KCan_Eval);}
#define ICan_Eval(N) (N)
#define FCan_Eval(N)

typedef struct REval *Eval;
#define KEval 6
#define NEval (IEval((Eval)N_INIT( \
	GetNode(sizeof(struct REval),KEval), \
	KEval,sizeof(struct REval))))
#define DEval(n) {FEval(n); FreeNode(n, KEval);}
#define IEval(N) (N)
#define FEval(N)

typedef struct RGrammar *Grammar;
#define KGrammar 8
#define NGrammar (IGrammar((Grammar)N_INIT( \
	GetNode(sizeof(struct RGrammar),KGrammar), \
	KGrammar,sizeof(struct RGrammar))))
#define DGrammar(n) {FGrammar(n); FreeNode(n, KGrammar);}
#define IGrammar(N) (N)
#define FGrammar(N)

typedef struct RGroup *Group;
#define KGroup 10
#define NGroup (IGroup((Group)N_INIT( \
	GetNode(sizeof(struct RGroup),KGroup), \
	KGroup,sizeof(struct RGroup))))
#define DGroup(n) {FGroup(n); FreeNode(n, KGroup);}
#define IGroup(N) (N)
#define FGroup(N)

typedef struct RMarked *Marked;
#define KMarked 12
#define NMarked (IMarked((Marked)N_INIT( \
	GetNode(sizeof(struct RMarked),KMarked), \
	KMarked,sizeof(struct RMarked))))
#define DMarked(n) {FMarked(n); FreeNode(n, KMarked);}
#define IMarked(N) (N)
#define FMarked(N)

typedef struct RNonterminal *Nonterminal;
#define KNonterminal 14
#define NNonterminal (INonterminal((Nonterminal)N_INIT( \
	GetNode(sizeof(struct RNonterminal),KNonterminal), \
	KNonterminal,sizeof(struct RNonterminal))))
#define DNonterminal(n) {FNonterminal(n); FreeNode(n, KNonterminal);}
#define INonterminal(N) (N)
#define FNonterminal(N)

typedef struct RRule *Rule;
#define KRule 16
#define NRule (IRule((Rule)N_INIT( \
	GetNode(sizeof(struct RRule),KRule), \
	KRule,sizeof(struct RRule))))
#define DRule(n) {FRule(n); FreeNode(n, KRule);}
#define IRule(N) (N)
#define FRule(N)

typedef struct RSymbols *Symbols;
#define KSymbols 18
#define NSymbols (ISymbols((Symbols)N_INIT( \
	GetNode(sizeof(struct RSymbols),KSymbols), \
	KSymbols,sizeof(struct RSymbols))))
#define DSymbols(n) {FSymbols(n); FreeNode(n, KSymbols);}
#define ISymbols(N) (N)
#define FSymbols(N)

typedef struct RTerminal *Terminal;
#define KTerminal 20
#define NTerminal (ITerminal((Terminal)N_INIT( \
	GetNode(sizeof(struct RTerminal),KTerminal), \
	KTerminal,sizeof(struct RTerminal))))
#define DTerminal(n) {FTerminal(n); FreeNode(n, KTerminal);}
#define ITerminal(N) (N)
#define FTerminal(N)

typedef struct RVSL *VSL;
#define KVSL 22
#define NVSL (IVSL((VSL)N_INIT( \
	GetNode(sizeof(struct RVSL),KVSL), \
	KVSL,sizeof(struct RVSL))))
#define DVSL(n) {FVSL(n); FreeNode(n, KVSL);}
#define IVSL(N) (N)
#define FVSL(N)

typedef struct RVisit_Seq *Visit_Seq;
#define KVisit_Seq 24
#define NVisit_Seq (IVisit_Seq((Visit_Seq)N_INIT( \
	GetNode(sizeof(struct RVisit_Seq),KVisit_Seq), \
	KVisit_Seq,sizeof(struct RVisit_Seq))))
#define DVisit_Seq(n) {FVisit_Seq(n); FreeNode(n, KVisit_Seq);}
#define IVisit_Seq(N) (N)
#define FVisit_Seq(N)

typedef struct RVisit_Sequences *Visit_Sequences;
#define KVisit_Sequences 26
#define NVisit_Sequences (IVisit_Sequences((Visit_Sequences)N_INIT( \
	GetNode(sizeof(struct RVisit_Sequences),KVisit_Sequences), \
	KVisit_Sequences,sizeof(struct RVisit_Sequences))))
#define DVisit_Sequences(n) {FVisit_Sequences(n); FreeNode(n, KVisit_Sequences);}
#define IVisit_Sequences(N) (N)
#define FVisit_Sequences(N)

typedef struct RVisits *Visits;
#define KVisits 28
#define NVisits (IVisits((Visits)N_INIT( \
	GetNode(sizeof(struct RVisits),KVisits), \
	KVisits,sizeof(struct RVisits))))
#define DVisits(n) {FVisits(n); FreeNode(n, KVisits);}
#define IVisits(N) (N)
#define FVisits(N)


/* Classes */
typedef union
{
   someptr IDLinternal;
   CPAction IDLclassCommon;
   Visits VVisits;
   Eval VEval;
} Action;

typedef union
{
   someptr IDLinternal;
   HgenericHeader IDLclassCommon;
   Terminal VTerminal;
   Nonterminal VNonterminal;
} Vocabulary;


/* Sets and Sequences */
typedef struct IDLtag_OPTIM1
{
   struct IDLtag_OPTIM1 *next;
   Vocabulary value;
} CVocabulary, *LVocabulary;

#define SEQVocabulary LVocabulary
#define inSEQVocabulary(Vocabularyseq,Vocabularyvalue) IDLInList((pGenList)Vocabularyseq,(Vocabularyvalue).IDLinternal)
#define initializeSEQVocabulary(Vocabularyseq) Vocabularyseq = NULL
#define appendfrontSEQVocabulary(Vocabularyseq,Vocabularyvalue) Vocabularyseq=\
		(SEQVocabulary)IDLListAddFront((pGenList)Vocabularyseq,(Vocabularyvalue).IDLinternal)
#define appendrearSEQVocabulary(Vocabularyseq,Vocabularyvalue) Vocabularyseq=\
		(SEQVocabulary)IDLListAddRear((pGenList)Vocabularyseq,(Vocabularyvalue).IDLinternal)
#define orderedinsertSEQVocabulary(Vocabularyseq,Vocabularyvalue,Vocabularycompfn) Vocabularyseq=\
		(SEQVocabulary)IDLListOrderedInsert((pGenList)Vocabularyseq,(Vocabularyvalue).IDLinternal,Vocabularycompfn)
#define retrievefirstSEQVocabulary(Vocabularyseq, Vocabularyvalue)\
 		Vocabularyvalue.IDLclassCommon = (HgenericHeader)IDLListRetrieveFirst((pGenList)Vocabularyseq)
#define retrievelastSEQVocabulary(Vocabularyseq, Vocabularyvalue)\
 		Vocabularyvalue.IDLclassCommon = (HgenericHeader)IDLListRetrieveLast((pGenList)Vocabularyseq)
#define ithinSEQVocabulary(Vocabularyseq, index, Vocabularyvalue)\
		Vocabularyvalue.IDLclassCommon = (HgenericHeader)IDLListRetrieveIth((pGenList)Vocabularyseq, index)
#define tailSEQVocabulary(Vocabularyseq)\
		((Vocabularyseq) ? Vocabularyseq->next : NULL)
#define removefirstSEQVocabulary(Vocabularyseq) Vocabularyseq=\
		(SEQVocabulary)IDLListRemoveFirstCell((pGenList)Vocabularyseq)
#define removeSEQVocabulary(Vocabularyseq,Vocabularyvalue) Vocabularyseq=\
		(SEQVocabulary)IDLListRemoveCell((pGenList)Vocabularyseq,(Vocabularyvalue).IDLinternal)
#define removelastSEQVocabulary(Vocabularyseq) Vocabularyseq=\
		(SEQVocabulary)IDLListRemoveLastCell((pGenList)Vocabularyseq)
#define foreachinSEQVocabulary(Vocabularyseq,Vocabularyptr,Vocabularyvalue) for\
(Vocabularyptr = Vocabularyseq; \
		Vocabularyptr!=NULL&&((Vocabularyvalue.IDLinternal=Vocabularyptr->value.IDLinternal)||1); \
Vocabularyptr=Vocabularyptr->next)
#define emptySEQVocabulary(Vocabularyseq) ((Vocabularyseq)==NULL)
#define lengthSEQVocabulary(Vocabularyseq) IDLListLength((someptr)Vocabularyseq)
#define copySEQVocabulary(Vocabularyseq) (SEQVocabulary)IDLListCopy(Vocabularyseq)
#define sortSEQVocabulary(Vocabularyseq, cmpfn) Vocabularyseq = (SEQVocabulary)IDLListSort(Vocabularyseq, cmpfn)

typedef struct IDLtag_OPTIM2
{
   struct IDLtag_OPTIM2 *next;
   Rule value;
} CRule, *LRule;

#define SEQRule LRule
#define inSEQRule(Ruleseq,Rulevalue) IDLInList((pGenList)Ruleseq,Rulevalue)
#define initializeSEQRule(Ruleseq) Ruleseq = NULL
#define appendfrontSEQRule(Ruleseq,Rulevalue) Ruleseq=\
		(SEQRule)IDLListAddFront((pGenList)Ruleseq,(someptr)Rulevalue)
#define appendrearSEQRule(Ruleseq,Rulevalue) Ruleseq=\
		(SEQRule)IDLListAddRear((pGenList)Ruleseq,(someptr)Rulevalue)
#define orderedinsertSEQRule(Ruleseq,Rulevalue,Rulecompfn) Ruleseq=\
		(SEQRule)IDLListOrderedInsert((pGenList)Ruleseq,Rulevalue,Rulecompfn)
#define retrievefirstSEQRule(Ruleseq, Rulevalue)\
 		Rulevalue = (Rule)IDLListRetrieveFirst((pGenList)Ruleseq)
#define retrievelastSEQRule(Ruleseq, Rulevalue)\
 		Rulevalue = (Rule)IDLListRetrieveLast((pGenList)Ruleseq)
#define ithinSEQRule(Ruleseq, index, Rulevalue)\
		Rulevalue = (Rule)IDLListRetrieveIth((pGenList)Ruleseq, index)
#define tailSEQRule(Ruleseq)\
		((Ruleseq) ? Ruleseq->next : NULL)
#define removefirstSEQRule(Ruleseq) Ruleseq=\
		(SEQRule)IDLListRemoveFirstCell((pGenList)Ruleseq)
#define removeSEQRule(Ruleseq,Rulevalue) Ruleseq=\
		(SEQRule)IDLListRemoveCell((pGenList)Ruleseq,(someptr)Rulevalue)
#define removelastSEQRule(Ruleseq) Ruleseq=\
		(SEQRule)IDLListRemoveLastCell((pGenList)Ruleseq)
#define foreachinSEQRule(Ruleseq,Ruleptr,Rulevalue) for\
(Ruleptr = Ruleseq; \
 		Ruleptr!=NULL&&((Rulevalue=Ruleptr->value)||1); \
Ruleptr=Ruleptr->next)
#define emptySEQRule(Ruleseq) ((Ruleseq)==NULL)
#define lengthSEQRule(Ruleseq) IDLListLength((someptr)Ruleseq)
#define copySEQRule(Ruleseq) (SEQRule)IDLListCopy(Ruleseq)
#define sortSEQRule(Ruleseq, cmpfn) Ruleseq = (SEQRule)IDLListSort(Ruleseq, cmpfn)

typedef struct IDLtag_OPTIM3
{
   struct IDLtag_OPTIM3 *next;
   Terminal value;
} CTerminal, *LTerminal;

#define SEQTerminal LTerminal
#define inSEQTerminal(Terminalseq,Terminalvalue) IDLInList((pGenList)Terminalseq,Terminalvalue)
#define initializeSEQTerminal(Terminalseq) Terminalseq = NULL
#define appendfrontSEQTerminal(Terminalseq,Terminalvalue) Terminalseq=\
		(SEQTerminal)IDLListAddFront((pGenList)Terminalseq,(someptr)Terminalvalue)
#define appendrearSEQTerminal(Terminalseq,Terminalvalue) Terminalseq=\
		(SEQTerminal)IDLListAddRear((pGenList)Terminalseq,(someptr)Terminalvalue)
#define orderedinsertSEQTerminal(Terminalseq,Terminalvalue,Terminalcompfn) Terminalseq=\
		(SEQTerminal)IDLListOrderedInsert((pGenList)Terminalseq,Terminalvalue,Terminalcompfn)
#define retrievefirstSEQTerminal(Terminalseq, Terminalvalue)\
 		Terminalvalue = (Terminal)IDLListRetrieveFirst((pGenList)Terminalseq)
#define retrievelastSEQTerminal(Terminalseq, Terminalvalue)\
 		Terminalvalue = (Terminal)IDLListRetrieveLast((pGenList)Terminalseq)
#define ithinSEQTerminal(Terminalseq, index, Terminalvalue)\
		Terminalvalue = (Terminal)IDLListRetrieveIth((pGenList)Terminalseq, index)
#define tailSEQTerminal(Terminalseq)\
		((Terminalseq) ? Terminalseq->next : NULL)
#define removefirstSEQTerminal(Terminalseq) Terminalseq=\
		(SEQTerminal)IDLListRemoveFirstCell((pGenList)Terminalseq)
#define removeSEQTerminal(Terminalseq,Terminalvalue) Terminalseq=\
		(SEQTerminal)IDLListRemoveCell((pGenList)Terminalseq,(someptr)Terminalvalue)
#define removelastSEQTerminal(Terminalseq) Terminalseq=\
		(SEQTerminal)IDLListRemoveLastCell((pGenList)Terminalseq)
#define foreachinSEQTerminal(Terminalseq,Terminalptr,Terminalvalue) for\
(Terminalptr = Terminalseq; \
 		Terminalptr!=NULL&&((Terminalvalue=Terminalptr->value)||1); \
Terminalptr=Terminalptr->next)
#define emptySEQTerminal(Terminalseq) ((Terminalseq)==NULL)
#define lengthSEQTerminal(Terminalseq) IDLListLength((someptr)Terminalseq)
#define copySEQTerminal(Terminalseq) (SEQTerminal)IDLListCopy(Terminalseq)
#define sortSEQTerminal(Terminalseq, cmpfn) Terminalseq = (SEQTerminal)IDLListSort(Terminalseq, cmpfn)

typedef struct IDLtag_OPTIM4
{
   struct IDLtag_OPTIM4 *next;
   Nonterminal value;
} CNonterminal, *LNonterminal;

#define SEQNonterminal LNonterminal
#define inSEQNonterminal(Nonterminalseq,Nonterminalvalue) IDLInList((pGenList)Nonterminalseq,Nonterminalvalue)
#define initializeSEQNonterminal(Nonterminalseq) Nonterminalseq = NULL
#define appendfrontSEQNonterminal(Nonterminalseq,Nonterminalvalue) Nonterminalseq=\
		(SEQNonterminal)IDLListAddFront((pGenList)Nonterminalseq,(someptr)Nonterminalvalue)
#define appendrearSEQNonterminal(Nonterminalseq,Nonterminalvalue) Nonterminalseq=\
		(SEQNonterminal)IDLListAddRear((pGenList)Nonterminalseq,(someptr)Nonterminalvalue)
#define orderedinsertSEQNonterminal(Nonterminalseq,Nonterminalvalue,Nonterminalcompfn) Nonterminalseq=\
		(SEQNonterminal)IDLListOrderedInsert((pGenList)Nonterminalseq,Nonterminalvalue,Nonterminalcompfn)
#define retrievefirstSEQNonterminal(Nonterminalseq, Nonterminalvalue)\
 		Nonterminalvalue = (Nonterminal)IDLListRetrieveFirst((pGenList)Nonterminalseq)
#define retrievelastSEQNonterminal(Nonterminalseq, Nonterminalvalue)\
 		Nonterminalvalue = (Nonterminal)IDLListRetrieveLast((pGenList)Nonterminalseq)
#define ithinSEQNonterminal(Nonterminalseq, index, Nonterminalvalue)\
		Nonterminalvalue = (Nonterminal)IDLListRetrieveIth((pGenList)Nonterminalseq, index)
#define tailSEQNonterminal(Nonterminalseq)\
		((Nonterminalseq) ? Nonterminalseq->next : NULL)
#define removefirstSEQNonterminal(Nonterminalseq) Nonterminalseq=\
		(SEQNonterminal)IDLListRemoveFirstCell((pGenList)Nonterminalseq)
#define removeSEQNonterminal(Nonterminalseq,Nonterminalvalue) Nonterminalseq=\
		(SEQNonterminal)IDLListRemoveCell((pGenList)Nonterminalseq,(someptr)Nonterminalvalue)
#define removelastSEQNonterminal(Nonterminalseq) Nonterminalseq=\
		(SEQNonterminal)IDLListRemoveLastCell((pGenList)Nonterminalseq)
#define foreachinSEQNonterminal(Nonterminalseq,Nonterminalptr,Nonterminalvalue) for\
(Nonterminalptr = Nonterminalseq; \
 		Nonterminalptr!=NULL&&((Nonterminalvalue=Nonterminalptr->value)||1); \
Nonterminalptr=Nonterminalptr->next)
#define emptySEQNonterminal(Nonterminalseq) ((Nonterminalseq)==NULL)
#define lengthSEQNonterminal(Nonterminalseq) IDLListLength((someptr)Nonterminalseq)
#define copySEQNonterminal(Nonterminalseq) (SEQNonterminal)IDLListCopy(Nonterminalseq)
#define sortSEQNonterminal(Nonterminalseq, cmpfn) Nonterminalseq = (SEQNonterminal)IDLListSort(Nonterminalseq, cmpfn)

typedef struct IDLtag_OPTIM5
{
   struct IDLtag_OPTIM5 *next;
   VSL value;
} CVSL, *LVSL;

#define SEQVSL LVSL
#define inSEQVSL(VSLseq,VSLvalue) IDLInList((pGenList)VSLseq,VSLvalue)
#define initializeSEQVSL(VSLseq) VSLseq = NULL
#define appendfrontSEQVSL(VSLseq,VSLvalue) VSLseq=\
		(SEQVSL)IDLListAddFront((pGenList)VSLseq,(someptr)VSLvalue)
#define appendrearSEQVSL(VSLseq,VSLvalue) VSLseq=\
		(SEQVSL)IDLListAddRear((pGenList)VSLseq,(someptr)VSLvalue)
#define orderedinsertSEQVSL(VSLseq,VSLvalue,VSLcompfn) VSLseq=\
		(SEQVSL)IDLListOrderedInsert((pGenList)VSLseq,VSLvalue,VSLcompfn)
#define retrievefirstSEQVSL(VSLseq, VSLvalue)\
 		VSLvalue = (VSL)IDLListRetrieveFirst((pGenList)VSLseq)
#define retrievelastSEQVSL(VSLseq, VSLvalue)\
 		VSLvalue = (VSL)IDLListRetrieveLast((pGenList)VSLseq)
#define ithinSEQVSL(VSLseq, index, VSLvalue)\
		VSLvalue = (VSL)IDLListRetrieveIth((pGenList)VSLseq, index)
#define tailSEQVSL(VSLseq)\
		((VSLseq) ? VSLseq->next : NULL)
#define removefirstSEQVSL(VSLseq) VSLseq=\
		(SEQVSL)IDLListRemoveFirstCell((pGenList)VSLseq)
#define removeSEQVSL(VSLseq,VSLvalue) VSLseq=\
		(SEQVSL)IDLListRemoveCell((pGenList)VSLseq,(someptr)VSLvalue)
#define removelastSEQVSL(VSLseq) VSLseq=\
		(SEQVSL)IDLListRemoveLastCell((pGenList)VSLseq)
#define foreachinSEQVSL(VSLseq,VSLptr,VSLvalue) for\
(VSLptr = VSLseq; \
 		VSLptr!=NULL&&((VSLvalue=VSLptr->value)||1); \
VSLptr=VSLptr->next)
#define emptySEQVSL(VSLseq) ((VSLseq)==NULL)
#define lengthSEQVSL(VSLseq) IDLListLength((someptr)VSLseq)
#define copySEQVSL(VSLseq) (SEQVSL)IDLListCopy(VSLseq)
#define sortSEQVSL(VSLseq, cmpfn) VSLseq = (SEQVSL)IDLListSort(VSLseq, cmpfn)

typedef struct IDLtag_OPTIM6
{
   struct IDLtag_OPTIM6 *next;
   Group value;
} CGroup, *LGroup;

#define SEQGroup LGroup
#define inSEQGroup(Groupseq,Groupvalue) IDLInList((pGenList)Groupseq,Groupvalue)
#define initializeSEQGroup(Groupseq) Groupseq = NULL
#define appendfrontSEQGroup(Groupseq,Groupvalue) Groupseq=\
		(SEQGroup)IDLListAddFront((pGenList)Groupseq,(someptr)Groupvalue)
#define appendrearSEQGroup(Groupseq,Groupvalue) Groupseq=\
		(SEQGroup)IDLListAddRear((pGenList)Groupseq,(someptr)Groupvalue)
#define orderedinsertSEQGroup(Groupseq,Groupvalue,Groupcompfn) Groupseq=\
		(SEQGroup)IDLListOrderedInsert((pGenList)Groupseq,Groupvalue,Groupcompfn)
#define retrievefirstSEQGroup(Groupseq, Groupvalue)\
 		Groupvalue = (Group)IDLListRetrieveFirst((pGenList)Groupseq)
#define retrievelastSEQGroup(Groupseq, Groupvalue)\
 		Groupvalue = (Group)IDLListRetrieveLast((pGenList)Groupseq)
#define ithinSEQGroup(Groupseq, index, Groupvalue)\
		Groupvalue = (Group)IDLListRetrieveIth((pGenList)Groupseq, index)
#define tailSEQGroup(Groupseq)\
		((Groupseq) ? Groupseq->next : NULL)
#define removefirstSEQGroup(Groupseq) Groupseq=\
		(SEQGroup)IDLListRemoveFirstCell((pGenList)Groupseq)
#define removeSEQGroup(Groupseq,Groupvalue) Groupseq=\
		(SEQGroup)IDLListRemoveCell((pGenList)Groupseq,(someptr)Groupvalue)
#define removelastSEQGroup(Groupseq) Groupseq=\
		(SEQGroup)IDLListRemoveLastCell((pGenList)Groupseq)
#define foreachinSEQGroup(Groupseq,Groupptr,Groupvalue) for\
(Groupptr = Groupseq; \
 		Groupptr!=NULL&&((Groupvalue=Groupptr->value)||1); \
Groupptr=Groupptr->next)
#define emptySEQGroup(Groupseq) ((Groupseq)==NULL)
#define lengthSEQGroup(Groupseq) IDLListLength((someptr)Groupseq)
#define copySEQGroup(Groupseq) (SEQGroup)IDLListCopy(Groupseq)
#define sortSEQGroup(Groupseq, cmpfn) Groupseq = (SEQGroup)IDLListSort(Groupseq, cmpfn)

typedef struct IDLtag_OPTIM7
{
   struct IDLtag_OPTIM7 *next;
   Marked value;
} CMarked, *LMarked;

#define SEQMarked LMarked
#define inSEQMarked(Markedseq,Markedvalue) IDLInList((pGenList)Markedseq,Markedvalue)
#define initializeSEQMarked(Markedseq) Markedseq = NULL
#define appendfrontSEQMarked(Markedseq,Markedvalue) Markedseq=\
		(SEQMarked)IDLListAddFront((pGenList)Markedseq,(someptr)Markedvalue)
#define appendrearSEQMarked(Markedseq,Markedvalue) Markedseq=\
		(SEQMarked)IDLListAddRear((pGenList)Markedseq,(someptr)Markedvalue)
#define orderedinsertSEQMarked(Markedseq,Markedvalue,Markedcompfn) Markedseq=\
		(SEQMarked)IDLListOrderedInsert((pGenList)Markedseq,Markedvalue,Markedcompfn)
#define retrievefirstSEQMarked(Markedseq, Markedvalue)\
 		Markedvalue = (Marked)IDLListRetrieveFirst((pGenList)Markedseq)
#define retrievelastSEQMarked(Markedseq, Markedvalue)\
 		Markedvalue = (Marked)IDLListRetrieveLast((pGenList)Markedseq)
#define ithinSEQMarked(Markedseq, index, Markedvalue)\
		Markedvalue = (Marked)IDLListRetrieveIth((pGenList)Markedseq, index)
#define tailSEQMarked(Markedseq)\
		((Markedseq) ? Markedseq->next : NULL)
#define removefirstSEQMarked(Markedseq) Markedseq=\
		(SEQMarked)IDLListRemoveFirstCell((pGenList)Markedseq)
#define removeSEQMarked(Markedseq,Markedvalue) Markedseq=\
		(SEQMarked)IDLListRemoveCell((pGenList)Markedseq,(someptr)Markedvalue)
#define removelastSEQMarked(Markedseq) Markedseq=\
		(SEQMarked)IDLListRemoveLastCell((pGenList)Markedseq)
#define foreachinSEQMarked(Markedseq,Markedptr,Markedvalue) for\
(Markedptr = Markedseq; \
 		Markedptr!=NULL&&((Markedvalue=Markedptr->value)||1); \
Markedptr=Markedptr->next)
#define emptySEQMarked(Markedseq) ((Markedseq)==NULL)
#define lengthSEQMarked(Markedseq) IDLListLength((someptr)Markedseq)
#define copySEQMarked(Markedseq) (SEQMarked)IDLListCopy(Markedseq)
#define sortSEQMarked(Markedseq, cmpfn) Markedseq = (SEQMarked)IDLListSort(Markedseq, cmpfn)

typedef struct IDLtag_OPTIM8
{
   struct IDLtag_OPTIM8 *next;
   Can_Eval value;
} CCan_Eval, *LCan_Eval;

#define SEQCan_Eval LCan_Eval
#define inSEQCan_Eval(Can_Evalseq,Can_Evalvalue) IDLInList((pGenList)Can_Evalseq,Can_Evalvalue)
#define initializeSEQCan_Eval(Can_Evalseq) Can_Evalseq = NULL
#define appendfrontSEQCan_Eval(Can_Evalseq,Can_Evalvalue) Can_Evalseq=\
		(SEQCan_Eval)IDLListAddFront((pGenList)Can_Evalseq,(someptr)Can_Evalvalue)
#define appendrearSEQCan_Eval(Can_Evalseq,Can_Evalvalue) Can_Evalseq=\
		(SEQCan_Eval)IDLListAddRear((pGenList)Can_Evalseq,(someptr)Can_Evalvalue)
#define orderedinsertSEQCan_Eval(Can_Evalseq,Can_Evalvalue,Can_Evalcompfn) Can_Evalseq=\
		(SEQCan_Eval)IDLListOrderedInsert((pGenList)Can_Evalseq,Can_Evalvalue,Can_Evalcompfn)
#define retrievefirstSEQCan_Eval(Can_Evalseq, Can_Evalvalue)\
 		Can_Evalvalue = (Can_Eval)IDLListRetrieveFirst((pGenList)Can_Evalseq)
#define retrievelastSEQCan_Eval(Can_Evalseq, Can_Evalvalue)\
 		Can_Evalvalue = (Can_Eval)IDLListRetrieveLast((pGenList)Can_Evalseq)
#define ithinSEQCan_Eval(Can_Evalseq, index, Can_Evalvalue)\
		Can_Evalvalue = (Can_Eval)IDLListRetrieveIth((pGenList)Can_Evalseq, index)
#define tailSEQCan_Eval(Can_Evalseq)\
		((Can_Evalseq) ? Can_Evalseq->next : NULL)
#define removefirstSEQCan_Eval(Can_Evalseq) Can_Evalseq=\
		(SEQCan_Eval)IDLListRemoveFirstCell((pGenList)Can_Evalseq)
#define removeSEQCan_Eval(Can_Evalseq,Can_Evalvalue) Can_Evalseq=\
		(SEQCan_Eval)IDLListRemoveCell((pGenList)Can_Evalseq,(someptr)Can_Evalvalue)
#define removelastSEQCan_Eval(Can_Evalseq) Can_Evalseq=\
		(SEQCan_Eval)IDLListRemoveLastCell((pGenList)Can_Evalseq)
#define foreachinSEQCan_Eval(Can_Evalseq,Can_Evalptr,Can_Evalvalue) for\
(Can_Evalptr = Can_Evalseq; \
 		Can_Evalptr!=NULL&&((Can_Evalvalue=Can_Evalptr->value)||1); \
Can_Evalptr=Can_Evalptr->next)
#define emptySEQCan_Eval(Can_Evalseq) ((Can_Evalseq)==NULL)
#define lengthSEQCan_Eval(Can_Evalseq) IDLListLength((someptr)Can_Evalseq)
#define copySEQCan_Eval(Can_Evalseq) (SEQCan_Eval)IDLListCopy(Can_Evalseq)
#define sortSEQCan_Eval(Can_Evalseq, cmpfn) Can_Evalseq = (SEQCan_Eval)IDLListSort(Can_Evalseq, cmpfn)

typedef struct IDLtag_OPTIM9
{
   struct IDLtag_OPTIM9 *next;
   Symbols value;
} CSymbols, *LSymbols;

#define SEQSymbols LSymbols
#define inSEQSymbols(Symbolsseq,Symbolsvalue) IDLInList((pGenList)Symbolsseq,Symbolsvalue)
#define initializeSEQSymbols(Symbolsseq) Symbolsseq = NULL
#define appendfrontSEQSymbols(Symbolsseq,Symbolsvalue) Symbolsseq=\
		(SEQSymbols)IDLListAddFront((pGenList)Symbolsseq,(someptr)Symbolsvalue)
#define appendrearSEQSymbols(Symbolsseq,Symbolsvalue) Symbolsseq=\
		(SEQSymbols)IDLListAddRear((pGenList)Symbolsseq,(someptr)Symbolsvalue)
#define orderedinsertSEQSymbols(Symbolsseq,Symbolsvalue,Symbolscompfn) Symbolsseq=\
		(SEQSymbols)IDLListOrderedInsert((pGenList)Symbolsseq,Symbolsvalue,Symbolscompfn)
#define retrievefirstSEQSymbols(Symbolsseq, Symbolsvalue)\
 		Symbolsvalue = (Symbols)IDLListRetrieveFirst((pGenList)Symbolsseq)
#define retrievelastSEQSymbols(Symbolsseq, Symbolsvalue)\
 		Symbolsvalue = (Symbols)IDLListRetrieveLast((pGenList)Symbolsseq)
#define ithinSEQSymbols(Symbolsseq, index, Symbolsvalue)\
		Symbolsvalue = (Symbols)IDLListRetrieveIth((pGenList)Symbolsseq, index)
#define tailSEQSymbols(Symbolsseq)\
		((Symbolsseq) ? Symbolsseq->next : NULL)
#define removefirstSEQSymbols(Symbolsseq) Symbolsseq=\
		(SEQSymbols)IDLListRemoveFirstCell((pGenList)Symbolsseq)
#define removeSEQSymbols(Symbolsseq,Symbolsvalue) Symbolsseq=\
		(SEQSymbols)IDLListRemoveCell((pGenList)Symbolsseq,(someptr)Symbolsvalue)
#define removelastSEQSymbols(Symbolsseq) Symbolsseq=\
		(SEQSymbols)IDLListRemoveLastCell((pGenList)Symbolsseq)
#define foreachinSEQSymbols(Symbolsseq,Symbolsptr,Symbolsvalue) for\
(Symbolsptr = Symbolsseq; \
 		Symbolsptr!=NULL&&((Symbolsvalue=Symbolsptr->value)||1); \
Symbolsptr=Symbolsptr->next)
#define emptySEQSymbols(Symbolsseq) ((Symbolsseq)==NULL)
#define lengthSEQSymbols(Symbolsseq) IDLListLength((someptr)Symbolsseq)
#define copySEQSymbols(Symbolsseq) (SEQSymbols)IDLListCopy(Symbolsseq)
#define sortSEQSymbols(Symbolsseq, cmpfn) Symbolsseq = (SEQSymbols)IDLListSort(Symbolsseq, cmpfn)

typedef struct IDLtag_OPTIM10
{
   struct IDLtag_OPTIM10 *next;
   Attributes value;
} CAttributes, *LAttributes;

#define SEQAttributes LAttributes
#define inSEQAttributes(Attributesseq,Attributesvalue) IDLInList((pGenList)Attributesseq,Attributesvalue)
#define initializeSEQAttributes(Attributesseq) Attributesseq = NULL
#define appendfrontSEQAttributes(Attributesseq,Attributesvalue) Attributesseq=\
		(SEQAttributes)IDLListAddFront((pGenList)Attributesseq,(someptr)Attributesvalue)
#define appendrearSEQAttributes(Attributesseq,Attributesvalue) Attributesseq=\
		(SEQAttributes)IDLListAddRear((pGenList)Attributesseq,(someptr)Attributesvalue)
#define orderedinsertSEQAttributes(Attributesseq,Attributesvalue,Attributescompfn) Attributesseq=\
		(SEQAttributes)IDLListOrderedInsert((pGenList)Attributesseq,Attributesvalue,Attributescompfn)
#define retrievefirstSEQAttributes(Attributesseq, Attributesvalue)\
 		Attributesvalue = (Attributes)IDLListRetrieveFirst((pGenList)Attributesseq)
#define retrievelastSEQAttributes(Attributesseq, Attributesvalue)\
 		Attributesvalue = (Attributes)IDLListRetrieveLast((pGenList)Attributesseq)
#define ithinSEQAttributes(Attributesseq, index, Attributesvalue)\
		Attributesvalue = (Attributes)IDLListRetrieveIth((pGenList)Attributesseq, index)
#define tailSEQAttributes(Attributesseq)\
		((Attributesseq) ? Attributesseq->next : NULL)
#define removefirstSEQAttributes(Attributesseq) Attributesseq=\
		(SEQAttributes)IDLListRemoveFirstCell((pGenList)Attributesseq)
#define removeSEQAttributes(Attributesseq,Attributesvalue) Attributesseq=\
		(SEQAttributes)IDLListRemoveCell((pGenList)Attributesseq,(someptr)Attributesvalue)
#define removelastSEQAttributes(Attributesseq) Attributesseq=\
		(SEQAttributes)IDLListRemoveLastCell((pGenList)Attributesseq)
#define foreachinSEQAttributes(Attributesseq,Attributesptr,Attributesvalue) for\
(Attributesptr = Attributesseq; \
 		Attributesptr!=NULL&&((Attributesvalue=Attributesptr->value)||1); \
Attributesptr=Attributesptr->next)
#define emptySEQAttributes(Attributesseq) ((Attributesseq)==NULL)
#define lengthSEQAttributes(Attributesseq) IDLListLength((someptr)Attributesseq)
#define copySEQAttributes(Attributesseq) (SEQAttributes)IDLListCopy(Attributesseq)
#define sortSEQAttributes(Attributesseq, cmpfn) Attributesseq = (SEQAttributes)IDLListSort(Attributesseq, cmpfn)

typedef struct IDLtag_OPTIM11
{
   struct IDLtag_OPTIM11 *next;
   Action value;
} CAction, *LAction;

#define SEQAction LAction
#define inSEQAction(Actionseq,Actionvalue) IDLInList((pGenList)Actionseq,(Actionvalue).IDLinternal)
#define initializeSEQAction(Actionseq) Actionseq = NULL
#define appendfrontSEQAction(Actionseq,Actionvalue) Actionseq=\
		(SEQAction)IDLListAddFront((pGenList)Actionseq,(Actionvalue).IDLinternal)
#define appendrearSEQAction(Actionseq,Actionvalue) Actionseq=\
		(SEQAction)IDLListAddRear((pGenList)Actionseq,(Actionvalue).IDLinternal)
#define orderedinsertSEQAction(Actionseq,Actionvalue,Actioncompfn) Actionseq=\
		(SEQAction)IDLListOrderedInsert((pGenList)Actionseq,(Actionvalue).IDLinternal,Actioncompfn)
#define retrievefirstSEQAction(Actionseq, Actionvalue)\
 		Actionvalue.IDLclassCommon = (CPAction)IDLListRetrieveFirst((pGenList)Actionseq)
#define retrievelastSEQAction(Actionseq, Actionvalue)\
 		Actionvalue.IDLclassCommon = (CPAction)IDLListRetrieveLast((pGenList)Actionseq)
#define ithinSEQAction(Actionseq, index, Actionvalue)\
		Actionvalue.IDLclassCommon = (CPAction)IDLListRetrieveIth((pGenList)Actionseq, index)
#define tailSEQAction(Actionseq)\
		((Actionseq) ? Actionseq->next : NULL)
#define removefirstSEQAction(Actionseq) Actionseq=\
		(SEQAction)IDLListRemoveFirstCell((pGenList)Actionseq)
#define removeSEQAction(Actionseq,Actionvalue) Actionseq=\
		(SEQAction)IDLListRemoveCell((pGenList)Actionseq,(Actionvalue).IDLinternal)
#define removelastSEQAction(Actionseq) Actionseq=\
		(SEQAction)IDLListRemoveLastCell((pGenList)Actionseq)
#define foreachinSEQAction(Actionseq,Actionptr,Actionvalue) for\
(Actionptr = Actionseq; \
		Actionptr!=NULL&&((Actionvalue.IDLinternal=Actionptr->value.IDLinternal)||1); \
Actionptr=Actionptr->next)
#define emptySEQAction(Actionseq) ((Actionseq)==NULL)
#define lengthSEQAction(Actionseq) IDLListLength((someptr)Actionseq)
#define copySEQAction(Actionseq) (SEQAction)IDLListCopy(Actionseq)
#define sortSEQAction(Actionseq, cmpfn) Actionseq = (SEQAction)IDLListSort(Actionseq, cmpfn)

typedef struct IDLtag_OPTIM12
{
   struct IDLtag_OPTIM12 *next;
   Visit_Seq value;
} CVisit_Seq, *LVisit_Seq;

#define SEQVisit_Seq LVisit_Seq
#define inSEQVisit_Seq(Visit_Seqseq,Visit_Seqvalue) IDLInList((pGenList)Visit_Seqseq,Visit_Seqvalue)
#define initializeSEQVisit_Seq(Visit_Seqseq) Visit_Seqseq = NULL
#define appendfrontSEQVisit_Seq(Visit_Seqseq,Visit_Seqvalue) Visit_Seqseq=\
		(SEQVisit_Seq)IDLListAddFront((pGenList)Visit_Seqseq,(someptr)Visit_Seqvalue)
#define appendrearSEQVisit_Seq(Visit_Seqseq,Visit_Seqvalue) Visit_Seqseq=\
		(SEQVisit_Seq)IDLListAddRear((pGenList)Visit_Seqseq,(someptr)Visit_Seqvalue)
#define orderedinsertSEQVisit_Seq(Visit_Seqseq,Visit_Seqvalue,Visit_Seqcompfn) Visit_Seqseq=\
		(SEQVisit_Seq)IDLListOrderedInsert((pGenList)Visit_Seqseq,Visit_Seqvalue,Visit_Seqcompfn)
#define retrievefirstSEQVisit_Seq(Visit_Seqseq, Visit_Seqvalue)\
 		Visit_Seqvalue = (Visit_Seq)IDLListRetrieveFirst((pGenList)Visit_Seqseq)
#define retrievelastSEQVisit_Seq(Visit_Seqseq, Visit_Seqvalue)\
 		Visit_Seqvalue = (Visit_Seq)IDLListRetrieveLast((pGenList)Visit_Seqseq)
#define ithinSEQVisit_Seq(Visit_Seqseq, index, Visit_Seqvalue)\
		Visit_Seqvalue = (Visit_Seq)IDLListRetrieveIth((pGenList)Visit_Seqseq, index)
#define tailSEQVisit_Seq(Visit_Seqseq)\
		((Visit_Seqseq) ? Visit_Seqseq->next : NULL)
#define removefirstSEQVisit_Seq(Visit_Seqseq) Visit_Seqseq=\
		(SEQVisit_Seq)IDLListRemoveFirstCell((pGenList)Visit_Seqseq)
#define removeSEQVisit_Seq(Visit_Seqseq,Visit_Seqvalue) Visit_Seqseq=\
		(SEQVisit_Seq)IDLListRemoveCell((pGenList)Visit_Seqseq,(someptr)Visit_Seqvalue)
#define removelastSEQVisit_Seq(Visit_Seqseq) Visit_Seqseq=\
		(SEQVisit_Seq)IDLListRemoveLastCell((pGenList)Visit_Seqseq)
#define foreachinSEQVisit_Seq(Visit_Seqseq,Visit_Seqptr,Visit_Seqvalue) for\
(Visit_Seqptr = Visit_Seqseq; \
 		Visit_Seqptr!=NULL&&((Visit_Seqvalue=Visit_Seqptr->value)||1); \
Visit_Seqptr=Visit_Seqptr->next)
#define emptySEQVisit_Seq(Visit_Seqseq) ((Visit_Seqseq)==NULL)
#define lengthSEQVisit_Seq(Visit_Seqseq) IDLListLength((someptr)Visit_Seqseq)
#define copySEQVisit_Seq(Visit_Seqseq) (SEQVisit_Seq)IDLListCopy(Visit_Seqseq)
#define sortSEQVisit_Seq(Visit_Seqseq, cmpfn) Visit_Seqseq = (SEQVisit_Seq)IDLListSort(Visit_Seqseq, cmpfn)


/* Class Attributes */
struct CAAction
{
   IDLnodeHeader IDLhidden;
   SEQAttributes birth;
   SEQAttributes obituary;
};

/* Node Structures */
struct RAttributes
{
   IDLnodeHeader IDLhidden;
   int symbno;
   int gen;
   int typeid;
   int part;
   int class;
   int symbid;
   int attrid;
   String sname;
   String aname;
   int death;
   int birth;
   Boolean not_used;
   Boolean BnNF;
   Boolean termattr;
};
struct RCan_Eval
{
   IDLnodeHeader IDLhidden;
   SEQMarked marks;
   SEQAction actions;
   int ord;
   int symbid;
};
struct REval
{
   IDLnodeHeader IDLhidden;
   SEQAttributes birth;
   SEQAttributes obituary;
   SEQAttributes params;
   int col;
   int row;
   int symbno;
   int attrid;
};
struct RGrammar
{
   IDLnodeHeader IDLhidden;
   Nonterminal startsymb;
   SEQRule rules;
   SEQTerminal terms;
   SEQNonterminal nonterms;
};
struct RGroup
{
   IDLnodeHeader IDLhidden;
   SEQAttributes members;
   int class;
   String name;
   int typeid;
   int groupid;
};
struct RMarked
{
   IDLnodeHeader IDLhidden;
   int ord;
   int symbid;
   Boolean flag;
};
struct RNonterminal
{
   IDLnodeHeader IDLhidden;
   int follow;
   int first;
   int index;
   int symbid;
};
struct RRule
{
   IDLnodeHeader IDLhidden;
   SEQVocabulary rhs;
   Nonterminal lhs;
   int prodid;
};
struct RSymbols
{
   IDLnodeHeader IDLhidden;
   int symbno;
   int parts;
   int symbid;
   String name;
};
struct RTerminal
{
   IDLnodeHeader IDLhidden;
   int follow;
   int first;
   int occ;
   int attrid;
};
struct RVSL
{
   IDLnodeHeader IDLhidden;
   SEQAttributes def_attrs;
   SEQAttributes app_attrs;
   SEQAction actions;
   int prodid;
};
struct RVisit_Seq
{
   IDLnodeHeader IDLhidden;
   SEQAction actions;
   SEQAttributes attrs;
   SEQSymbols symbs;
   int prodid;
};
struct RVisit_Sequences
{
   IDLnodeHeader IDLhidden;
   Grammar lt_grammar;
   SEQVSL vsl;
   SEQGroup groups;
   SEQAttributes attrs;
   SEQSymbols symbs;
   SEQAttributes tree_attrs;
   SEQAttributes glob_var;
   SEQAttributes single_visit;
   SEQCan_Eval caneval;
   SEQVisit_Seq vi_seq;
};
struct RVisits
{
   IDLnodeHeader IDLhidden;
   SEQAttributes birth;
   SEQAttributes obituary;
   int symbid;
   int ord;
   int symbno;
};

/* Port Declarations */
void optout();
Visit_Sequences optin();
