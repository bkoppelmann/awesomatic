
#include "HEAD.h"
#include "err.h"
#include "node.h"
#include "visitprocs.h"
#include "attrpredef.h"

#include "visitmap.h"

#ifdef MONITOR
#include "attr_mon_dapto.h"
#include "liga_dapto.h"
#endif

#ifndef _VisitVarDecl
#define _VisitVarDecl()
#endif

#ifndef _VisitEntry
#define _VisitEntry()
#endif

#ifndef _VisitExit
#define _VisitExit()
#endif


#if defined(__STDC__) || defined(__cplusplus)
#define _CALL_VS_(args) (void (*)args)
#else
#define _CALL_VS_(args) 
#endif
DefTableKey* _IG_incl40;
int* _IG_incl39;
DefTableKeyList* _IG_incl38;
DefTableKey* _IG_incl37;
int* _IG_incl36;
DefTableKey* _IG_incl35;
Environment* _IG_incl34;
int* _IG_incl33;
int* _IG_incl31;
int* _IG_incl32;
DefTableKey* _IG_incl30;
int* _IG_incl29;
Environment* _IG_incl27;
Environment* _IG_incl26;
Environment* _IG_incl25;
Environment* _IG_incl23;
Environment* _IG_incl22;
Environment* _IG_incl21;
Environment* _IG_incl20;
DefTableKey* _IG_incl19;
Environment* _IG_incl18;
int* _IG_incl14;
int* _IG_incl6;
DefTableKey* _IG_incl8;
DefTableKey* _IG_incl4;
PTGNode _AVSpec__const20;
PExprListPtr _AVParam_cPExprListPtr_post;
DefTableKeyListPtr _AVSymbolDefId_cDefTableKeyListPtr_post;
PTGNode _AVComputations__const20;
PTGNode _AVSpecs__const20;
int _AVParam_PExprTakeIt;
PExpr _AVParam_PExprElem;
PExprListPtr _AVParamsOpt_HEAD$47_RuleAttr_203;
PExprList _AVParamsOpt_PExprList;
int _AVDepAttr_PExprTakeIt;
PExpr _AVDepAttr_PExprElem;
PExprList _AVDepClause__PExprauxList;
PExprList _AVDepClause_PExprList;
PExpr _AVExpression_repr;
int _AVExpandOpt_Fct0;
int _AVExpandOpt_Fct1;
int _AVExpandOpt_Fct2;
DefTableKey _AVExpandOpt_Type;
int _AVSubtree_SubtreeNo;
PExpr _AVRemoteExpression_repr;
int _AVRemoteExpression_SubtreeNo;
SymbAttrList _AVRemoteClause_RemoteSet;
DefTableKeyList _AVShield_ShieldSet;
Binding _AVAttrComp__const3;
PExpr _AVAttrComp_repr;
DefTableKey _AVAttrComp_IsChainStart;
PExpr _AVDefAttr_repr;
int _AVDefAttr_hasAccuToken;
int _AVDefAttr_IsUpperSymbComp;
Environment _AVDefAttr_CompScope;
PTGNode _AVCompute_InhComps;
int _AVCompute_Sym;
PExpr _AVPlainComp_repr;
Environment _AVPlainComp_CompScope;
Binding _AVAttrName_Bind;
PExpr _AVLoop_repr;
int _AVAttrDefId_Sym;
Binding _AVChainName_Bind;
int _AVSymOcc_IsDefining;
int _AVSymOcc_AttrClass;
int _AVAttr_IsIterate;
Binding _AVAttrUseId_auxChainBind_RuleAttr_149;
DefTableKey _AVAttrUseId_auxChainKey_RuleAttr_149;
Binding _AVAttrUseId_Bind;
DefTableKey _AVAttrUseId_ScopeKey;
ProdSymbolListPtr _AVSyntLit_cProdSymbolListPtr_post;
DefTableKey _AVRuleSpecId_Key;
PTGNode _AVRuleSpec__const20;
RuleProd _AVRuleSpec_Rule;
Binding _AVSyntUnit_newGenTreeAttr_RuleAttr_160;
int _AVSyntLit_ProdSymbolTakeIt;
ProdSymbol _AVSyntLit_ProdSymbolElem;
ProdSymbolList _AVProduction__ProdSymbolauxList;
int _AVProduction_IsListof;
ProdSymbolList _AVProduction_ProdSymbolList;
PTGNode _AVCompPart__const20;
DefTableKeyList _AVSymbolDefIds__DefTableKeyauxList;
PTGNode _AVSymCompSpec__const20;
DefTableKeyList _AVSymCompSpec__DefTableKeyauxList_RuleAttr_163;
int _AVSymCompSpec_isRoot;
ProdSymbol _AVSymbolRef_ProdSymbol_RuleAttr_167;
int _AVSymbolRef_occs_RuleAttr_167;
PExpr _AVSymbolRef_repr;
int _AVSymbolRef_Pos;
DefTableKey _AVSymbolRef_Key;
PTGNode _AVAG_TargetComps;
PTGNode _AVAG_TargetDefs;
Binding _AVRuleId_Bind;
Binding _AVTypeId_Bind;
int _AVSyntId_ProdSymbolTakeIt;
Binding _AVSyntId_Bind;
ProdSymbol _AVSyntId_ProdSymbolElem;
int _AVSyntId_IsGenSymbol;
int _AVSymbolDefId_DefTableKeyTakeIt;
Binding _AVSymbolDefId_Bind;
DefTableKey _AVSymbolDefId_DefTableKeyElem;

#if defined(__STDC__) || defined(__cplusplus)
void LIGA_ATTREVAL (NODEPTR _currn)
#else
void LIGA_ATTREVAL (_currn) NODEPTR _currn;
#endif
{(*(VS1MAP[_currn->_prod])) ((NODEPTR)_currn);}
/*SPC(0)*/

#if defined(__STDC__) || defined(__cplusplus)
void _VS0Empty(NODEPTR _currn)
#else
void _VS0Empty(_currn) NODEPTR _currn;
#endif
{ _VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_1(_TPPrule_1 _currn)
#else
void _VS1rule_1(_currn )
_TPPrule_1 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATisAccu=1;
/*SPC(2108)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_2(_TPPrule_2 _currn)
#else
void _VS1rule_2(_currn )
_TPPrule_2 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATisAccu=0;
/*SPC(2107)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_3(_TPPrule_3 _currn)
#else
void _VS1rule_3(_currn )
_TPPrule_3 _currn;

#endif
{
PExpr _AS3repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_3(_TPPrule_3 _currn)
#else
void _VS2rule_3(_currn )
_TPPrule_3 _currn;

#endif
{
PExpr _AS3repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_3(_TPPrule_3 _currn)
#else
void _VS3rule_3(_currn )
_TPPrule_3 _currn;

#endif
{
PExpr _AS3repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_3(_TPPrule_3 _currn)
#else
void _VS4rule_3(_currn )
_TPPrule_3 _currn;

#endif
{
PExpr _AS3repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (AND(_currn->_desc2->_ATisAccu, GetIsChain(_currn->_desc1->_ATAttrKey, 0))) {
message(ERROR, CatStrInd("CHAIN computation can not be accumulating: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(2082)*/
_AVAttrComp__const3=_currn->_desc1->_ATBind;
/*SPC(2071)*/
_currn->_AT_const4=_AVDefAttr_IsUpperSymbComp;
/*SPC(2071)*/
_currn->_ATisAccu=AND(_currn->_desc2->_ATisAccu, NOT(GetIsChain(_currn->_desc1->_ATAttrKey, 0)));
/*SPC(2072)*/
_AVAttrComp_IsChainStart=NoKey;
/*SPC(1654)*/

if (_currn->_ATisAccu) {
ResetHasAccuAsgn(_currn->_desc1->_ATAttrKey, 1);

} else {
ResetHasNonAccuAsgn(_currn->_desc1->_ATAttrKey, 1);
}
;
/*SPC(2084)*/

if (AND(_currn->_ATisAccu, NE(GetAttrType(_currn->_desc1->_ATAttrKey, VOIDkey), VOIDkey))) {
message(ERROR, CatStrInd("Accumulating attribute must have type VOID: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(2099)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_3(_TPPrule_3 _currn)
#else
void _VS5rule_3(_currn )
_TPPrule_3 _currn;

#endif
{
PExpr _AS3repr;

_VisitVarDecl()
_VisitEntry();

if (AND(_currn->_ATisAccu, GetHasNonAccuAsgn(_currn->_desc1->_ATAttrKey, 0))) {
message(ERROR, CatStrInd("Collides with a non-accumulating computation in this context: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(2093)*/
_AVDefAttr_hasAccuToken=_currn->_desc2->_ATisAccu;
/*SPC(2076)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3,(&( _AS3repr)));
_AVAttrComp_repr=newAssign(_AVDefAttr_repr, _AS3repr, (&( _currn->_AT_pos)));
/*SPC(1856)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_4(_TPPrule_4 _currn)
#else
void _VS1rule_4(_currn )
_TPPrule_4 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATrepr=newRHS(_currn->_ATTERM_1, (&( _currn->_AT_pos)));
/*SPC(1963)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_5(_TPPrule_5 _currn)
#else
void _VS1rule_5(_currn )
_TPPrule_5 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_5(_TPPrule_5 _currn)
#else
void _VS4rule_5(_currn )
_TPPrule_5 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVExpression_repr=newIntValue(_currn->_ATTERM_1, (&( _currn->_AT_pos)));
/*SPC(1948)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_6(_TPPrule_6 _currn)
#else
void _VS1rule_6(_currn )
_TPPrule_6 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_6(_TPPrule_6 _currn)
#else
void _VS4rule_6(_currn )
_TPPrule_6 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVExpression_repr=newFltValue(_currn->_ATTERM_1, (&( _currn->_AT_pos)));
/*SPC(1945)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_7(_TPPrule_7 _currn)
#else
void _VS1rule_7(_currn )
_TPPrule_7 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_7(_TPPrule_7 _currn)
#else
void _VS4rule_7(_currn )
_TPPrule_7 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVExpression_repr=newStrValue(_currn->_ATTERM_1, (&( _currn->_AT_pos)));
/*SPC(1942)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_8(_TPPrule_8 _currn)
#else
void _VS1rule_8(_currn )
_TPPrule_8 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_8(_TPPrule_8 _currn)
#else
void _VS4rule_8(_currn )
_TPPrule_8 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVExpression_repr=newChrValue(_currn->_ATTERM_1, (&( _currn->_AT_pos)));
/*SPC(1939)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_9(_TPPrule_9 _currn)
#else
void _VS1rule_9(_currn )
_TPPrule_9 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_9(_TPPrule_9 _currn)
#else
void _VS2rule_9(_currn )
_TPPrule_9 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_9(_TPPrule_9 _currn)
#else
void _VS4rule_9(_currn )
_TPPrule_9 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVExpression_repr=_AVSymbolRef_repr;
/*SPC(1936)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_10(_TPPrule_10 _currn)
#else
void _VS1rule_10(_currn )
_TPPrule_10 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_10(_TPPrule_10 _currn)
#else
void _VS2rule_10(_currn )
_TPPrule_10 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_10(_TPPrule_10 _currn)
#else
void _VS3rule_10(_currn )
_TPPrule_10 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_10(_TPPrule_10 _currn)
#else
void _VS4rule_10(_currn )
_TPPrule_10 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVExpression_repr=_AVRemoteExpression_repr;
/*SPC(1933)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_11(_TPPrule_11 _currn)
#else
void _VS1rule_11(_currn )
_TPPrule_11 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_11(_TPPrule_11 _currn)
#else
void _VS4rule_11(_currn )
_TPPrule_11 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVExpression_repr=_currn->_desc1->_ATrepr;
/*SPC(1930)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_12(_TPPrule_12 _currn)
#else
void _VS1rule_12(_currn )
_TPPrule_12 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_12(_TPPrule_12 _currn)
#else
void _VS2rule_12(_currn )
_TPPrule_12 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_12(_TPPrule_12 _currn)
#else
void _VS3rule_12(_currn )
_TPPrule_12 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
_currn->_desc1->_ATIsDefining=0;
/*SPC(1405)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_12(_TPPrule_12 _currn)
#else
void _VS4rule_12(_currn )
_TPPrule_12 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
_AVAttr_IsIterate=0;
/*SPC(985)*/
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1repr)));
_AVExpression_repr=_AS1repr;
/*SPC(1927)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_13(_TPPrule_13 _currn)
#else
void _VS1rule_13(_currn )
_TPPrule_13 _currn;

#endif
{
PExprList _AS1_PExprauxList;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_13(_TPPrule_13 _currn)
#else
void _VS2rule_13(_currn )
_TPPrule_13 _currn;

#endif
{
PExprList _AS1_PExprauxList;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_13(_TPPrule_13 _currn)
#else
void _VS3rule_13(_currn )
_TPPrule_13 _currn;

#endif
{
PExprList _AS1_PExprauxList;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_13(_TPPrule_13 _currn)
#else
void _VS4rule_13(_currn )
_TPPrule_13 _currn;

#endif
{
PExprList _AS1_PExprauxList;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR ,PExprList*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1_PExprauxList)));
_AVExpression_repr=newCall(_currn->_ATTERM_1, _AVParamsOpt_PExprList, (&( _currn->_AT_pos)));
/*SPC(1918)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_14(_TPPrule_14 _currn)
#else
void _VS1rule_14(_currn )
_TPPrule_14 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_14(_TPPrule_14 _currn)
#else
void _VS2rule_14(_currn )
_TPPrule_14 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_14(_TPPrule_14 _currn)
#else
void _VS3rule_14(_currn )
_TPPrule_14 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_14(_TPPrule_14 _currn,PExpr* _AS0repr)
#else
void _VS4rule_14(_currn ,_AS0repr)
_TPPrule_14 _currn;
PExpr* _AS0repr;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(* _AS0repr)=newDepend(_AVExpression_repr, _AVDepClause_PExprList, (&( _currn->_AT_pos)));
/*SPC(1906)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_15(_TPPrule_15 _currn)
#else
void _VS1rule_15(_currn )
_TPPrule_15 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_15(_TPPrule_15 _currn)
#else
void _VS2rule_15(_currn )
_TPPrule_15 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_15(_TPPrule_15 _currn)
#else
void _VS3rule_15(_currn )
_TPPrule_15 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_15(_TPPrule_15 _currn,PExpr* _AS0repr)
#else
void _VS4rule_15(_currn ,_AS0repr)
_TPPrule_15 _currn;
PExpr* _AS0repr;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(* _AS0repr)=_AVExpression_repr;
/*SPC(1902)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_16(_TPPrule_16 _currn)
#else
void _VS2rule_16(_currn )
_TPPrule_16 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVExpandOpt_Fct0=NoStrIndex;
/*SPC(1827)*/
_AVExpandOpt_Fct1=NoStrIndex;
/*SPC(1826)*/
_AVExpandOpt_Fct2=NoStrIndex;
/*SPC(1825)*/
_AVExpandOpt_Type=NoKey;
/*SPC(1824)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_17(_TPPrule_17 _currn)
#else
void _VS1rule_17(_currn )
_TPPrule_17 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_17(_TPPrule_17 _currn)
#else
void _VS2rule_17(_currn )
_TPPrule_17 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsSymbol(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as symbol identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(463)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVExpandOpt_Fct0=_currn->_ATTERM_3;
/*SPC(1820)*/
_AVExpandOpt_Fct1=_currn->_ATTERM_2;
/*SPC(1819)*/
_AVExpandOpt_Fct2=_currn->_ATTERM_1;
/*SPC(1818)*/
_AVExpandOpt_Type=_currn->_desc1->_ATKey;
/*SPC(1817)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_18(_TPPrule_18 _currn)
#else
void _VS1rule_18(_currn )
_TPPrule_18 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_18(_TPPrule_18 _currn)
#else
void _VS2rule_18(_currn )
_TPPrule_18 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_18(_TPPrule_18 _currn)
#else
void _VS3rule_18(_currn )
_TPPrule_18 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSymOcc_IsDefining=0;
/*SPC(1227)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVSubtree_SubtreeNo=PosOfProdSymbol(_currn->_desc1->_ATProdSymbol);
/*SPC(1808)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_19(_TPPrule_19 _currn)
#else
void _VS1rule_19(_currn )
_TPPrule_19 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_19(_TPPrule_19 _currn)
#else
void _VS2rule_19(_currn )
_TPPrule_19 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_19(_TPPrule_19 _currn)
#else
void _VS3rule_19(_currn )
_TPPrule_19 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_19(_TPPrule_19 _currn)
#else
void _VS4rule_19(_currn )
_TPPrule_19 _currn;

#endif
{
DefTableKey* _IL_incl19;

_VisitVarDecl()
_VisitEntry();
_IL_incl19=_IG_incl19;_IG_incl19= &(_currn->_ATRemoteKey);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_currn->_ATRemoteKey=EnterSglConstit(_AVRemoteClause_RemoteSet, _AVShield_ShieldSet, (* _IG_incl4), (&( _currn->_AT_pos)));
/*SPC(1798)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_AVRemoteExpression_SubtreeNo=_AVSubtree_SubtreeNo;
/*SPC(1797)*/
_AVRemoteExpression_repr=
((GetIsIncluding(_currn->_ATRemoteKey, 0)
) ? (newIncluding(_currn->_ATRemoteKey, (&( _currn->_AT_pos)))
) : (
((GetIsConstituent(_currn->_ATRemoteKey, 0)
) ? (newConstituent(_currn->_ATRemoteKey, _AVRemoteExpression_SubtreeNo, (&( _currn->_AT_pos)))
) : (
((GetIsConstituents(_currn->_ATRemoteKey, 0)
) ? (newConstituents(_currn->_ATRemoteKey, _AVRemoteExpression_SubtreeNo, (&( _currn->_AT_pos)))
) : (wrongExpr))
))
))
;
/*SPC(1952)*/
_IG_incl19=_IL_incl19;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_20(_TPPrule_20 _currn)
#else
void _VS1rule_20(_currn )
_TPPrule_20 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc4->_prod])))((NODEPTR) _currn->_desc4);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_20(_TPPrule_20 _currn)
#else
void _VS2rule_20(_currn )
_TPPrule_20 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_20(_TPPrule_20 _currn)
#else
void _VS3rule_20(_currn )
_TPPrule_20 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_20(_TPPrule_20 _currn)
#else
void _VS4rule_20(_currn )
_TPPrule_20 _currn;

#endif
{
DefTableKey* _IL_incl19;

_VisitVarDecl()
_VisitEntry();
_IL_incl19=_IG_incl19;_IG_incl19= &(_currn->_ATRemoteKey);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc4->_prod])))((NODEPTR) _currn->_desc4);
_currn->_ATRemoteKey=EnterMulConstit(_AVRemoteClause_RemoteSet, _AVShield_ShieldSet, _AVExpandOpt_Type, _AVExpandOpt_Fct2, _AVExpandOpt_Fct1, _AVExpandOpt_Fct0, (* _IG_incl4), (&( _currn->_AT_pos)));
/*SPC(1788)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_AVRemoteExpression_SubtreeNo=_AVSubtree_SubtreeNo;
/*SPC(1787)*/
_AVRemoteExpression_repr=
((GetIsIncluding(_currn->_ATRemoteKey, 0)
) ? (newIncluding(_currn->_ATRemoteKey, (&( _currn->_AT_pos)))
) : (
((GetIsConstituent(_currn->_ATRemoteKey, 0)
) ? (newConstituent(_currn->_ATRemoteKey, _AVRemoteExpression_SubtreeNo, (&( _currn->_AT_pos)))
) : (
((GetIsConstituents(_currn->_ATRemoteKey, 0)
) ? (newConstituents(_currn->_ATRemoteKey, _AVRemoteExpression_SubtreeNo, (&( _currn->_AT_pos)))
) : (wrongExpr))
))
))
;
/*SPC(1952)*/
_IG_incl19=_IL_incl19;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_21(_TPPrule_21 _currn)
#else
void _VS1rule_21(_currn )
_TPPrule_21 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_21(_TPPrule_21 _currn)
#else
void _VS2rule_21(_currn )
_TPPrule_21 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_21(_TPPrule_21 _currn)
#else
void _VS3rule_21(_currn )
_TPPrule_21 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_21(_TPPrule_21 _currn)
#else
void _VS4rule_21(_currn )
_TPPrule_21 _currn;

#endif
{
DefTableKey* _IL_incl19;

_VisitVarDecl()
_VisitEntry();
_IL_incl19=_IG_incl19;_IG_incl19= &(_currn->_ATRemoteKey);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_currn->_ATRemoteKey=EnterIncluding(_AVRemoteClause_RemoteSet, (* _IG_incl4), (&( _currn->_AT_pos)));
/*SPC(1781)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVRemoteExpression_SubtreeNo=0;
/*SPC(1805)*/
_AVRemoteExpression_repr=
((GetIsIncluding(_currn->_ATRemoteKey, 0)
) ? (newIncluding(_currn->_ATRemoteKey, (&( _currn->_AT_pos)))
) : (
((GetIsConstituent(_currn->_ATRemoteKey, 0)
) ? (newConstituent(_currn->_ATRemoteKey, _AVRemoteExpression_SubtreeNo, (&( _currn->_AT_pos)))
) : (
((GetIsConstituents(_currn->_ATRemoteKey, 0)
) ? (newConstituents(_currn->_ATRemoteKey, _AVRemoteExpression_SubtreeNo, (&( _currn->_AT_pos)))
) : (wrongExpr))
))
))
;
/*SPC(1952)*/
_IG_incl19=_IL_incl19;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_22(_TPPrule_22 _currn)
#else
void _VS3rule_22(_currn )
_TPPrule_22 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
UnmarkSymbols();
/*SPC(1712)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVShield_ShieldSet=MakeShieldSet();
/*SPC(1715)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_23(_TPPrule_23 _currn)
#else
void _VS3rule_23(_currn )
_TPPrule_23 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVShield_ShieldSet=NULLDefTableKeyList;
/*SPC(1707)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_24(_TPPrule_24 _currn)
#else
void _VS3rule_24(_currn )
_TPPrule_24 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_desc1->_ATIsBottomUp=1;
/*SPC(1672)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_24(_TPPrule_24 _currn)
#else
void _VS4rule_24(_currn )
_TPPrule_24 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_24(_TPPrule_24 _currn,PTGNode* _AS0_const20)
#else
void _VS5rule_24(_currn ,_AS0_const20)
_TPPrule_24 _currn;
PTGNode* _AS0_const20;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(* _AS0_const20)=IDENTICAL(_AVCompute_InhComps);
/*SPC(1671)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_25(_TPPrule_25 _currn)
#else
void _VS1rule_25(_currn )
_TPPrule_25 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_25(_TPPrule_25 _currn)
#else
void _VS2rule_25(_currn )
_TPPrule_25 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_25(_TPPrule_25 _currn)
#else
void _VS3rule_25(_currn )
_TPPrule_25 _currn;

#endif
{
int* _IL_incl33;

_VisitVarDecl()
_VisitEntry();
_IL_incl33=_IG_incl33;_IG_incl33= &(_currn->_ATIsBottomUp);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_IG_incl33=_IL_incl33;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_25(_TPPrule_25 _currn)
#else
void _VS4rule_25(_currn )
_TPPrule_25 _currn;

#endif
{
int* _IL_incl33;
DefTableKey* _IL_incl4;

_VisitVarDecl()
_VisitEntry();
_IL_incl33=_IG_incl33;_IG_incl33= &(_currn->_ATIsBottomUp);
_IL_incl4=_IG_incl4;_IG_incl4= &(_currn->_ATKey);
_currn->_ATisAccu=0;
/*SPC(2109)*/
_currn->_ATBind=_currn->_desc1->_ATBind;
/*SPC(1657)*/
_currn->_ATKey=KeyOf(_currn->_ATBind);
/*SPC(1662)*/
ResetIsAccu(_currn->_ATKey, _currn->_ATisAccu);
/*SPC(2053)*/
ResetContext(_currn->_ATKey, (* _IG_incl8));
ResetIsBottomUp(_currn->_ATKey, _currn->_ATIsBottomUp);
ResetIsRuleComputation(_currn->_ATKey, (* _IG_incl6));
;
/*SPC(1664)*/
_IG_incl33=_IL_incl33;
_IG_incl4=_IL_incl4;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_25(_TPPrule_25 _currn)
#else
void _VS5rule_25(_currn )
_TPPrule_25 _currn;

#endif
{
int* _IL_incl33;
int* _IL_incl14;
DefTableKey* _IL_incl4;

_VisitVarDecl()
_VisitEntry();
_IL_incl33=_IG_incl33;_IG_incl33= &(_currn->_ATIsBottomUp);
_IL_incl14=_IG_incl14;_IG_incl14= &(_currn->_ATIsUpperSymbComp);
_IL_incl4=_IG_incl4;_IG_incl4= &(_currn->_ATKey);
_currn->_ATIsUpperSymbComp=0;
/*SPC(1658)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (_currn->_ATisAccu) {
AccumulateExpr(_currn->_ATKey, _AVPlainComp_repr);

} else {
ResetCompRepr(_currn->_ATKey, _AVPlainComp_repr);
}
;
/*SPC(1847)*/
_AVCompute_InhComps=
(((* _IG_incl6)
) ? (PTGNULL
) : (InhCompOutput(_currn->_ATKey)))
;
/*SPC(1696)*/
_AVCompute_Sym=IdnOf(_currn->_ATBind);
/*SPC(1663)*/
_IG_incl33=_IL_incl33;
_IG_incl14=_IL_incl14;
_IG_incl4=_IL_incl4;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_26(_TPPrule_26 _currn)
#else
void _VS1rule_26(_currn )
_TPPrule_26 _currn;

#endif
{
PExpr _AS2repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_26(_TPPrule_26 _currn)
#else
void _VS2rule_26(_currn )
_TPPrule_26 _currn;

#endif
{
PExpr _AS2repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_26(_TPPrule_26 _currn)
#else
void _VS3rule_26(_currn )
_TPPrule_26 _currn;

#endif
{
PExpr _AS2repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_26(_TPPrule_26 _currn)
#else
void _VS4rule_26(_currn )
_TPPrule_26 _currn;

#endif
{
PExpr _AS2repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (NOT(GetIsChain(_currn->_desc1->_ATAttrKey, 0))) {
message(ERROR, "CHAINSTART must assign a chain", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1651)*/
_AVAttrComp__const3=_currn->_desc1->_ATBind;
/*SPC(1647)*/
_currn->_AT_const4=_AVDefAttr_IsUpperSymbComp;
/*SPC(1647)*/
_currn->_ATisAccu=0;
/*SPC(2110)*/
_AVAttrComp_IsChainStart=_currn->_desc1->_ATAttrKey;
/*SPC(1648)*/
ResetHasNonAccuAsgn(_currn->_desc1->_ATAttrKey, 1);
/*SPC(2103)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_26(_TPPrule_26 _currn)
#else
void _VS5rule_26(_currn )
_TPPrule_26 _currn;

#endif
{
PExpr _AS2repr;

_VisitVarDecl()
_VisitEntry();
_AVDefAttr_hasAccuToken=0;
/*SPC(2062)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2,(&( _AS2repr)));
_AVAttrComp_repr=newAssign(_AVDefAttr_repr, _AS2repr, (&( _currn->_AT_pos)));
/*SPC(1856)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_27(_TPPrule_27 _currn)
#else
void _VS1rule_27(_currn )
_TPPrule_27 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_27(_TPPrule_27 _currn)
#else
void _VS2rule_27(_currn )
_TPPrule_27 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_27(_TPPrule_27 _currn)
#else
void _VS3rule_27(_currn )
_TPPrule_27 _currn;

#endif
{
int* _IL_incl33;

_VisitVarDecl()
_VisitEntry();
_IL_incl33=_IG_incl33;_IG_incl33= &(_currn->_ATIsBottomUp);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_IG_incl33=_IL_incl33;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_27(_TPPrule_27 _currn)
#else
void _VS4rule_27(_currn )
_TPPrule_27 _currn;

#endif
{
int* _IL_incl33;
DefTableKey* _IL_incl4;

_VisitVarDecl()
_VisitEntry();
_IL_incl33=_IG_incl33;_IG_incl33= &(_currn->_ATIsBottomUp);
_IL_incl4=_IG_incl4;_IG_incl4= &(_currn->_ATKey);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_currn->_ATisAccu=_currn->_desc1->_ATisAccu;
/*SPC(2057)*/
_currn->_ATBind=
(ResetIsChainStart(KeyOf(_AVAttrComp__const3), _AVAttrComp_IsChainStart), _AVAttrComp__const3)
;
/*SPC(1639)*/
_currn->_ATKey=KeyOf(_currn->_ATBind);
/*SPC(1662)*/
ResetIsAccu(_currn->_ATKey, _currn->_ATisAccu);
/*SPC(2053)*/
ResetContext(_currn->_ATKey, (* _IG_incl8));
ResetIsBottomUp(_currn->_ATKey, _currn->_ATIsBottomUp);
ResetIsRuleComputation(_currn->_ATKey, (* _IG_incl6));
;
/*SPC(1664)*/
_IG_incl33=_IL_incl33;
_IG_incl4=_IL_incl4;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_27(_TPPrule_27 _currn)
#else
void _VS5rule_27(_currn )
_TPPrule_27 _currn;

#endif
{
int* _IL_incl33;
int* _IL_incl14;
DefTableKey* _IL_incl4;

_VisitVarDecl()
_VisitEntry();
_IL_incl33=_IG_incl33;_IG_incl33= &(_currn->_ATIsBottomUp);
_IL_incl14=_IG_incl14;_IG_incl14= &(_currn->_ATIsUpperSymbComp);
_IL_incl4=_IG_incl4;_IG_incl4= &(_currn->_ATKey);
_currn->_ATIsUpperSymbComp=_currn->_desc1->_AT_const4;
/*SPC(1644)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (_currn->_ATisAccu) {
AccumulateExpr(_currn->_ATKey, _AVAttrComp_repr);

} else {
ResetCompRepr(_currn->_ATKey, _AVAttrComp_repr);
}
;
/*SPC(1847)*/
_AVCompute_InhComps=
(((* _IG_incl6)
) ? (PTGNULL
) : (InhCompOutput(_currn->_ATKey)))
;
/*SPC(1696)*/
_AVCompute_Sym=IdnOf(_currn->_ATBind);
/*SPC(1663)*/
_IG_incl33=_IL_incl33;
_IG_incl14=_IL_incl14;
_IG_incl4=_IL_incl4;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_28(_TPPrule_28 _currn)
#else
void _VS1rule_28(_currn )
_TPPrule_28 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_28(_TPPrule_28 _currn)
#else
void _VS2rule_28(_currn )
_TPPrule_28 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
ResetIsSymbol(_currn->_desc1->_ATKey, 1);
/*SPC(439)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_28(_TPPrule_28 _currn)
#else
void _VS3rule_28(_currn )
_TPPrule_28 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (AND(NOT(GetIsDefined(_currn->_desc1->_ATKey, 0)), NOT(GetIsDefinedReported(_currn->_desc1->_ATKey, 0)))) {
ResetIsDefinedReported(_currn->_desc1->_ATKey, 1);
message(ERROR, CatStrInd("Symbol does not occur in rule or definition: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(1446)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
MarkInheritingTreeSymbs(_currn->_desc1->_ATBind);
/*SPC(1720)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_29(_TPPrule_29 _currn)
#else
void _VS1rule_29(_currn )
_TPPrule_29 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATValue=0;
/*SPC(1427)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_30(_TPPrule_30 _currn)
#else
void _VS1rule_30(_currn )
_TPPrule_30 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATValue=
((LE(_currn->_ATTERM_1, 0)
) ? (1
) : (_currn->_ATTERM_1))
;
/*SPC(1418)*/

if (LE(_currn->_ATTERM_1, 0)) {
message(ERROR, "Index must be greater than 0", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1423)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_31(_TPPrule_31 _currn)
#else
void _VS1rule_31(_currn )
_TPPrule_31 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_31(_TPPrule_31 _currn)
#else
void _VS2rule_31(_currn )
_TPPrule_31 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_31(_TPPrule_31 _currn)
#else
void _VS3rule_31(_currn )
_TPPrule_31 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
_currn->_desc1->_ATIsDefining=1;
/*SPC(1412)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_31(_TPPrule_31 _currn)
#else
void _VS4rule_31(_currn )
_TPPrule_31 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
_currn->_ATSym=_currn->_desc1->_AT_const7;
/*SPC(1552)*/
_currn->_ATAttrKey=_currn->_desc1->_AT_const8;
/*SPC(1551)*/
_AVDefAttr_IsUpperSymbComp=
((OR((* _IG_incl6), OR(GetIsChain(_currn->_ATAttrKey, 0), EQ(GetAttrClass(_currn->_ATAttrKey, NoClass), SYNTClass)))
) ? (0
) : (1))
;
/*SPC(1554)*/
_AVDefAttr_CompScope=
(((* _IG_incl6)
) ? (
((EQ(_currn->_desc1->_ATProdSymbol, NoProdSymbol)
) ? (GetLowerScope((* _IG_incl8), NoEnv)
) : (GetSymbolOccScope(_currn->_desc1->_ATProdSymbol)))

) : (
((_currn->_desc1->_ATIsHEADAcc
) ? (GetHEADScope((* _IG_incl8), NoEnv)
) : (
((_AVDefAttr_IsUpperSymbComp
) ? (GetUpperScope((* _IG_incl8), NoEnv)
) : (GetLowerScope((* _IG_incl8), NoEnv)))
))
))
;
/*SPC(1561)*/
_currn->_ATBind=AddAttrToBinding(AddCoordToBinding(BindIdn(_AVDefAttr_CompScope, _currn->_ATSym), (&( _currn->_AT_pos))), _currn->_ATAttrKey);
/*SPC(1573)*/
SetCheckMult(KeyOf(_currn->_ATBind), 1, 2);
/*SPC(1590)*/

if (AND(NOT((* _IG_incl6)), AND(NOT(GetIsChain(_currn->_ATAttrKey, 0)), EQ(GetAttrClass(_currn->_ATAttrKey, NoClass), NoClass)))) {
message(ERROR, CatStrInd("Class of defined attribute not specified: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1588)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_31(_TPPrule_31 _currn)
#else
void _VS5rule_31(_currn )
_TPPrule_31 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();

if (AND(NOT(_AVDefAttr_hasAccuToken), GetInhAccu(_currn->_ATAttrKey, 0))) {
message(ERROR, CatStrInd("Accumulating computation expected for attribute: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(2068)*/

if (AND(GT(GetCheckMult(KeyOf(_currn->_ATBind), 1), 1), AND(NOT(_AVDefAttr_hasAccuToken), NOT(GetHasAccuAsgn(_currn->_ATAttrKey, 0))))) {
message(ERROR, CatStrInd("Multiple computations of: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1598)*/
_AVAttr_IsIterate=0;
/*SPC(985)*/
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1repr)));
_AVDefAttr_repr=_AS1repr;
/*SPC(1898)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_32(_TPPrule_32 _currn)
#else
void _VS1rule_32(_currn )
_TPPrule_32 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_32(_TPPrule_32 _currn)
#else
void _VS3rule_32(_currn )
_TPPrule_32 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if ((* _IG_incl6)) {
message(ERROR, "TERM is not allowed in RULE computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1394)*/

if ((* _IG_incl14)) {
message(ERROR, "TERM is not allowed in upper symbol computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1399)*/
_AVSymbolRef_Key=NoKey;
/*SPC(1389)*/
_AVSymbolRef_Pos=_currn->_desc1->_ATValue;
/*SPC(1388)*/
_AVSymbolRef_repr=newSymbTermAcc(_currn->_desc1->_ATValue, (&( _currn->_AT_pos)));
/*SPC(1973)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_33(_TPPrule_33 _currn)
#else
void _VS1rule_33(_currn )
_TPPrule_33 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_33(_TPPrule_33 _currn)
#else
void _VS2rule_33(_currn )
_TPPrule_33 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
ResetIsSymbol(_currn->_desc1->_ATKey, 1);
/*SPC(439)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_33(_TPPrule_33 _currn)
#else
void _VS3rule_33(_currn )
_TPPrule_33 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATKey=_currn->_desc1->_ATKey;
/*SPC(1291)*/
_currn->_ATProdSymbol=
(((* _IG_incl6)
) ? (FindSymbolOcc(GetRule((* _IG_incl8), NoRuleProd), _currn->_ATKey, _currn->_desc2->_ATValue, (&( _currn->_AT_pos)))
) : (NoProdSymbol))
;
/*SPC(1299)*/
_AVSymOcc_AttrClass=
((OR(EQ(_currn->_ATProdSymbol, NoProdSymbol), NOT(_AVSymOcc_IsDefining))
) ? (NoClass
) : (
((EQ(PosOfProdSymbol(_currn->_ATProdSymbol), 0)
) ? (SYNTClass
) : (INHClass))
))
;
/*SPC(1309)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_33(_TPPrule_33 _currn)
#else
void _VS4rule_33(_currn )
_TPPrule_33 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (NOT((* _IG_incl6))) {
message(ERROR, "Not allowed in SYMBOL computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1293)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_34(_TPPrule_34 _currn)
#else
void _VS1rule_34(_currn )
_TPPrule_34 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_34(_TPPrule_34 _currn)
#else
void _VS3rule_34(_currn )
_TPPrule_34 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATKey=TAILKey;
/*SPC(1272)*/
_currn->_ATProdSymbol=
(((* _IG_incl6)
) ? (FindTAILSymbolOcc(GetRule((* _IG_incl8), NoRuleProd))
) : (NoProdSymbol))
;
/*SPC(1275)*/
_AVSymOcc_AttrClass=NoClass;
/*SPC(1228)*/

if (AND((* _IG_incl6), EQ(_currn->_ATProdSymbol, NoProdSymbol))) {
message(ERROR, "TAIL not allowed in RULE without nonterminal", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1286)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_35(_TPPrule_35 _currn)
#else
void _VS1rule_35(_currn )
_TPPrule_35 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_35(_TPPrule_35 _currn)
#else
void _VS3rule_35(_currn )
_TPPrule_35 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATKey=HEADKey;
/*SPC(1253)*/
_currn->_ATProdSymbol=
(((* _IG_incl6)
) ? (FindHEADSymbolOcc(GetRule((* _IG_incl8), NoRuleProd))
) : (NoProdSymbol))
;
/*SPC(1256)*/
_AVSymOcc_AttrClass=NoClass;
/*SPC(1228)*/

if (AND((* _IG_incl6), EQ(_currn->_ATProdSymbol, NoProdSymbol))) {
message(ERROR, "HEAD not allowed in RULE without nonterminal", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1268)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_36(_TPPrule_36 _currn)
#else
void _VS1rule_36(_currn )
_TPPrule_36 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_36(_TPPrule_36 _currn)
#else
void _VS3rule_36(_currn )
_TPPrule_36 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATKey=
(((* _IG_incl6)
) ? (NoKey
) : ((* _IG_incl8)))
;
/*SPC(1230)*/
_currn->_ATProdSymbol=NoProdSymbol;
/*SPC(1239)*/
_AVSymOcc_AttrClass=INHClass;
/*SPC(1248)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_36(_TPPrule_36 _currn)
#else
void _VS4rule_36(_currn )
_TPPrule_36 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if ((* _IG_incl6)) {
message(ERROR, "Not allowed in RULE computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1233)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_37(_TPPrule_37 _currn)
#else
void _VS1rule_37(_currn )
_TPPrule_37 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_37(_TPPrule_37 _currn)
#else
void _VS3rule_37(_currn )
_TPPrule_37 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATKey=
(((* _IG_incl6)
) ? (NoKey
) : ((* _IG_incl8)))
;
/*SPC(1230)*/
_currn->_ATProdSymbol=NoProdSymbol;
/*SPC(1239)*/
_AVSymOcc_AttrClass=SYNTClass;
/*SPC(1244)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_37(_TPPrule_37 _currn)
#else
void _VS4rule_37(_currn )
_TPPrule_37 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if ((* _IG_incl6)) {
message(ERROR, "Not allowed in RULE computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1233)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_38(_TPPrule_38 _currn)
#else
void _VS1rule_38(_currn )
_TPPrule_38 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_38(_TPPrule_38 _currn)
#else
void _VS2rule_38(_currn )
_TPPrule_38 _currn;

#endif
{
DefTableKey* _IL_incl35;

_VisitVarDecl()
_VisitEntry();
_IL_incl35=_IG_incl35;_IG_incl35= &(_currn->_ATType);
_currn->_ATType=_currn->_desc2->_ATKey;
/*SPC(1105)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_IG_incl35=_IL_incl35;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_38(_TPPrule_38 _currn)
#else
void _VS3rule_38(_currn )
_TPPrule_38 _currn;

#endif
{
DefTableKey* _IL_incl35;

_VisitVarDecl()
_VisitEntry();
_IL_incl35=_IG_incl35;_IG_incl35= &(_currn->_ATType);

if (GetIsSymbol(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as symbol identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(463)*/

if (GetIsRule(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_IG_incl35=_IL_incl35;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_39(_TPPrule_39 _currn)
#else
void _VS1rule_39(_currn )
_TPPrule_39 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_39(_TPPrule_39 _currn)
#else
void _VS2rule_39(_currn )
_TPPrule_39 _currn;

#endif
{
DefTableKey* _IL_incl37;
int* _IL_incl36;

_VisitVarDecl()
_VisitEntry();
_IL_incl37=_IG_incl37;_IG_incl37= &(_currn->_ATType);
_IL_incl36=_IG_incl36;_IG_incl36= &(_currn->_ATAttrClass);
_currn->_ATAttrClass=_currn->_desc3->_ATAttrClass;
/*SPC(1079)*/
_currn->_ATType=_currn->_desc2->_ATKey;
/*SPC(1078)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_IG_incl37=_IL_incl37;
_IG_incl36=_IL_incl36;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_39(_TPPrule_39 _currn)
#else
void _VS3rule_39(_currn )
_TPPrule_39 _currn;

#endif
{
DefTableKey* _IL_incl37;
int* _IL_incl36;

_VisitVarDecl()
_VisitEntry();
_IL_incl37=_IG_incl37;_IG_incl37= &(_currn->_ATType);
_IL_incl36=_IG_incl36;_IG_incl36= &(_currn->_ATAttrClass);

if (GetIsSymbol(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as symbol identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(463)*/

if (GetIsRule(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_IG_incl37=_IL_incl37;
_IG_incl36=_IL_incl36;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_40(_TPPrule_40 _currn)
#else
void _VS1rule_40(_currn )
_TPPrule_40 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATAttrClass=NoClass;
/*SPC(1020)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_41(_TPPrule_41 _currn)
#else
void _VS1rule_41(_currn )
_TPPrule_41 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATAttrClass=INHClass;
/*SPC(1017)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_42(_TPPrule_42 _currn)
#else
void _VS1rule_42(_currn )
_TPPrule_42 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATAttrClass=SYNTClass;
/*SPC(1014)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_43(_TPPrule_43 _currn)
#else
void _VS1rule_43(_currn )
_TPPrule_43 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_43(_TPPrule_43 _currn)
#else
void _VS2rule_43(_currn )
_TPPrule_43 _currn;

#endif
{
DefTableKey* _IL_incl40;
int* _IL_incl39;

_VisitVarDecl()
_VisitEntry();
_IL_incl40=_IG_incl40;_IG_incl40= &(_currn->_ATAttrType);
_IL_incl39=_IG_incl39;_IG_incl39= &(_currn->_ATAttrClass);
_currn->_ATAttrClass=_currn->_desc3->_ATAttrClass;
/*SPC(1011)*/
_currn->_ATAttrType=_currn->_desc2->_ATKey;
/*SPC(1010)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_IG_incl40=_IL_incl40;
_IG_incl39=_IL_incl39;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_43(_TPPrule_43 _currn)
#else
void _VS3rule_43(_currn )
_TPPrule_43 _currn;

#endif
{
DefTableKey* _IL_incl40;
int* _IL_incl39;

_VisitVarDecl()
_VisitEntry();
_IL_incl40=_IG_incl40;_IG_incl40= &(_currn->_ATAttrType);
_IL_incl39=_IG_incl39;_IG_incl39= &(_currn->_ATAttrClass);

if (GetIsSymbol(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as symbol identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(463)*/

if (GetIsRule(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_IG_incl40=_IL_incl40;
_IG_incl39=_IL_incl39;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_44(_TPPrule_44 _currn)
#else
void _VS1rule_44(_currn )
_TPPrule_44 _currn;

#endif
{
PExpr _AS3repr;
PExpr _AS2repr;
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_44(_TPPrule_44 _currn)
#else
void _VS2rule_44(_currn )
_TPPrule_44 _currn;

#endif
{
PExpr _AS3repr;
PExpr _AS2repr;
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_44(_TPPrule_44 _currn)
#else
void _VS3rule_44(_currn )
_TPPrule_44 _currn;

#endif
{
PExpr _AS3repr;
PExpr _AS2repr;
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_currn->_desc2->_ATIsDefining=0;
/*SPC(1405)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_44(_TPPrule_44 _currn)
#else
void _VS4rule_44(_currn )
_TPPrule_44 _currn;

#endif
{
PExpr _AS3repr;
PExpr _AS2repr;
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1repr)));
_AVAttr_IsIterate=1;
/*SPC(982)*/
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2,(&( _AS2repr)));
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3,(&( _AS3repr)));
_AVLoop_repr=newLoop(_AS1repr, _AS2repr, _AS3repr, (&( _currn->_AT_pos)));
/*SPC(1889)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_45(_TPPrule_45 _currn)
#else
void _VS1rule_45(_currn )
_TPPrule_45 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVAttrUseId_auxChainBind_RuleAttr_149=BindingInScope((* _IG_incl18), _currn->_ATSym);
/*SPC(938)*/
_AVAttrUseId_auxChainKey_RuleAttr_149=
((EQ(_AVAttrUseId_auxChainBind_RuleAttr_149, NoBinding)
) ? (NoKey
) : (KeyOf(_AVAttrUseId_auxChainBind_RuleAttr_149)))
;
/*SPC(941)*/
_AVAttrUseId_Bind=
((EQ(_AVAttrUseId_auxChainKey_RuleAttr_149, NoKey)
) ? (SetCoordSymToBinding(BindIdn(GetAttrScope(_AVAttrUseId_ScopeKey, NoEnv), _currn->_ATSym), (&( _currn->_AT_pos)), _currn->_ATSym)
) : (_AVAttrUseId_auxChainBind_RuleAttr_149))
;
/*SPC(944)*/
_currn->_ATKey=KeyOf(_AVAttrUseId_Bind);
/*SPC(954)*/

if (AND(OR(EQ(_AVAttrUseId_ScopeKey, HEADKey), EQ(_AVAttrUseId_ScopeKey, TAILKey)), EQ(_AVAttrUseId_auxChainKey_RuleAttr_149, NoKey))) {
message(ERROR, CatStrInd("HEAD and TAIL require a CHAIN name: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(962)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_46(_TPPrule_46 _currn)
#else
void _VS1rule_46(_currn )
_TPPrule_46 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_46(_TPPrule_46 _currn)
#else
void _VS3rule_46(_currn )
_TPPrule_46 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (NOT((* _IG_incl6))) {
message(ERROR, "Not allowed in SYMBOL computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(883)*/
_AVAttrUseId_ScopeKey=(* _IG_incl8);
/*SPC(878)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (GetIsChain(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Rule attribute may not have a CHAIN name: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(970)*/
_currn->_AT_const7=_currn->_desc1->_ATSym;
/*SPC(877)*/
_currn->_AT_const8=_currn->_desc1->_ATKey;
/*SPC(877)*/
_currn->_ATIsHEADAcc=0;
/*SPC(1547)*/
_currn->_ATProdSymbol=NoProdSymbol;
/*SPC(1546)*/
SetChkAttrClass(_currn->_desc1->_ATKey, _currn->_desc1->_ATSym, SYNTClass, (&( _currn->_AT_pos)));
/*SPC(901)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_46(_TPPrule_46 _currn,PExpr* _AS0repr)
#else
void _VS4rule_46(_currn ,_AS0repr)
_TPPrule_46 _currn;
PExpr* _AS0repr;

#endif
{

_VisitVarDecl()
_VisitEntry();
(* _AS0repr)=newRuleAttrAcc(_currn->_desc1->_ATKey, (&( _currn->_AT_pos)));
/*SPC(1996)*/

if (AND(_currn->_ATIsDefining, NOT(GetIsChain(_currn->_desc1->_ATKey, 0)))) {
ResetIsDefined(_currn->_desc1->_ATKey, 1);

} else {
}
;
/*SPC(1123)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_47(_TPPrule_47 _currn)
#else
void _VS1rule_47(_currn )
_TPPrule_47 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_47(_TPPrule_47 _currn)
#else
void _VS2rule_47(_currn )
_TPPrule_47 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_47(_TPPrule_47 _currn)
#else
void _VS3rule_47(_currn )
_TPPrule_47 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSymOcc_IsDefining=_currn->_ATIsDefining;
/*SPC(1409)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (AND((* _IG_incl6), GetIsTerm(_currn->_desc1->_ATKey, 0))) {
message(ERROR, "A terminal has no attributes", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(874)*/

if (AND(_currn->_ATIsDefining, EQ(_currn->_desc1->_ATKey, TAILKey))) {
message(ERROR, "TAIL must not be defined", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1539)*/

if (AND(NOT(_currn->_ATIsDefining), EQ(_currn->_desc1->_ATKey, HEADKey))) {
message(ERROR, "HEAD must not be used", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1542)*/
_AVAttrUseId_ScopeKey=_currn->_desc1->_ATKey;
/*SPC(867)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

if (AND(AND(_currn->_ATIsDefining, EQ(_AVSymOcc_AttrClass, INHClass)), AND(NOT((* _IG_incl6)), GetIsChain(_currn->_desc2->_ATKey, 0)))) {
message(ERROR, "CHAIN definition not allowed in upper symbol computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(921)*/
_currn->_AT_const7=_currn->_desc2->_ATSym;
/*SPC(866)*/
_currn->_AT_const8=_currn->_desc2->_ATKey;
/*SPC(866)*/
_currn->_ATIsHEADAcc=EQ(_currn->_desc1->_ATKey, HEADKey);
/*SPC(1536)*/
_currn->_ATProdSymbol=_currn->_desc1->_ATProdSymbol;
/*SPC(1535)*/

if (AND(NE(_AVSymOcc_AttrClass, NoClass), NOT(GetIsChain(_currn->_desc2->_ATKey, 0)))) {
SetChkAttrClass(_currn->_desc2->_ATKey, _currn->_desc2->_ATSym, _AVSymOcc_AttrClass, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(907)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_47(_TPPrule_47 _currn,PExpr* _AS0repr)
#else
void _VS4rule_47(_currn ,_AS0repr)
_TPPrule_47 _currn;
PExpr* _AS0repr;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (AND(_AVAttr_IsIterate, GetIsChain(_currn->_desc2->_ATKey, 0))) {
message(ERROR, CatStrInd("ITERATE attribute may not have a CHAIN name: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(992)*/

if (AND(NOT((* _IG_incl6)), AND(GetIsChain(_currn->_desc2->_ATKey, 0), (* _IG_incl14)))) {
message(ERROR, CatStrInd("chain access in upper symbol computation: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1614)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(* _AS0repr)=
((EQ(_currn->_desc1->_ATKey, HEADKey)
) ? (newHeadAcc(_currn->_desc2->_ATKey, (&( _currn->_AT_pos)))
) : (
((EQ(_currn->_desc1->_ATKey, TAILKey)
) ? (newTailAcc(_currn->_desc2->_ATKey, (&( _currn->_AT_pos)))
) : (
(((* _IG_incl6)
) ? (newAttrAccRule(_currn->_desc1->_ATProdSymbol, _currn->_desc2->_ATKey, GetIsChain(_currn->_desc2->_ATKey, 0), (&( _currn->_AT_pos)))
) : (newAttrAccSymb(_currn->_desc1->_ATKey, _currn->_desc2->_ATKey, GetIsChain(_currn->_desc2->_ATKey, 0), (&( _currn->_AT_pos)))))
))
))
;
/*SPC(1978)*/

if (AND(_currn->_ATIsDefining, NOT(GetIsChain(_currn->_desc2->_ATKey, 0)))) {
ResetIsDefined(_currn->_desc2->_ATKey, 1);

} else {
}
;
/*SPC(1123)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_48(_TPPrule_48 _currn)
#else
void _VS1rule_48(_currn )
_TPPrule_48 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_48(_TPPrule_48 _currn)
#else
void _VS2rule_48(_currn )
_TPPrule_48 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_48(_TPPrule_48 _currn)
#else
void _VS3rule_48(_currn )
_TPPrule_48 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVAttrUseId_ScopeKey=_currn->_desc1->_ATKey;
/*SPC(862)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

if (GetIsChain(_currn->_desc2->_ATKey, 0)) {
message(ERROR, "Remote access to a CHAIN not allowed", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(977)*/
ResetIsSymbol(_currn->_desc1->_ATKey, 1);
/*SPC(439)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_48(_TPPrule_48 _currn)
#else
void _VS4rule_48(_currn )
_TPPrule_48 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATuniqueRemAttr_RuleAttr_152=MarkInhTreeSymbAttrs(_currn->_desc1->_ATBind, _currn->_desc2->_ATSym);
/*SPC(1738)*/

if (EQ(_currn->_ATuniqueRemAttr_RuleAttr_152, 0)) {
message(ERROR, CatStrInd("Different attributes of one symbol are not allowed: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1747)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_48(_TPPrule_48 _currn)
#else
void _VS5rule_48(_currn )
_TPPrule_48 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (AND(NOT(GetIsDefined(_currn->_desc1->_ATKey, 0)), NOT(GetIsDefinedReported(_currn->_desc1->_ATKey, 0)))) {
ResetIsDefinedReported(_currn->_desc1->_ATKey, 1);
message(ERROR, CatStrInd("Symbol does not occur in rule or definition: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(1446)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (LT(_currn->_ATuniqueRemAttr_RuleAttr_152, 0)) {
ResetRemoteEpxrIsErr((* _IG_incl19), 1);
message(ERROR, CatStrInd("A terminal inherits this symbol: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;

if (GetIsTerm(KeyOf(_currn->_desc1->_ATBind), 0)) {
ResetRemoteEpxrIsErr((* _IG_incl19), 1);
message(ERROR, CatStrInd("Remote access to a terminal symbol: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
;
/*SPC(1749)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_49(_TPPrule_49 _currn)
#else
void _VS1rule_49(_currn )
_TPPrule_49 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSyntId_IsGenSymbol=0;
/*SPC(583)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_49(_TPPrule_49 _currn)
#else
void _VS2rule_49(_currn )
_TPPrule_49 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_49(_TPPrule_49 _currn)
#else
void _VS3rule_49(_currn )
_TPPrule_49 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsTerm(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("LISTOF element may not be a terminal symbol: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(812)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_50(_TPPrule_50 _currn)
#else
void _VS1rule_50(_currn )
_TPPrule_50 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_50(_TPPrule_50 _currn)
#else
void _VS2rule_50(_currn )
_TPPrule_50 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_50(_TPPrule_50 _currn)
#else
void _VS3rule_50(_currn )
_TPPrule_50 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (NOT(InheritClass((* _IG_incl20), GetAttrScope(_currn->_desc1->_ATKey, NoEnv)))) {
message(ERROR, "Cyclic inheritance", 0, (&( _currn->_AT_pos)));

} else {
InheritClass((* _IG_incl21), GetLowerScope(_currn->_desc1->_ATKey, NoEnv));
InheritClass((* _IG_incl22), GetUpperScope(_currn->_desc1->_ATKey, NoEnv));
InheritClass((* _IG_incl23), GetHEADScope(_currn->_desc1->_ATKey, NoEnv));
;
}
;
/*SPC(1191)*/
ResetIsSymbol(_currn->_desc1->_ATKey, 1);
/*SPC(439)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_50(_TPPrule_50 _currn)
#else
void _VS4rule_50(_currn )
_TPPrule_50 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsTreeSym(_currn->_desc1->_ATKey, 0)) {
message(WARNING, CatStrInd("INHERITS from a TREE symbol: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(700)*/

if (AND(NOT(GetIsDefined(_currn->_desc1->_ATKey, 0)), NOT(GetIsDefinedReported(_currn->_desc1->_ATKey, 0)))) {
ResetIsDefinedReported(_currn->_desc1->_ATKey, 1);
message(ERROR, CatStrInd("Symbol does not occur in rule or definition: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(1446)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_51(_TPPrule_51 _currn)
#else
void _VS1rule_51(_currn )
_TPPrule_51 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVProduction__ProdSymbolauxList=NULLProdSymbolList;
/*SPC(111)*/
_AVSyntLit_cProdSymbolListPtr_post=_ProdSymbolListADDROF(_AVProduction__ProdSymbolauxList);
/*SPC(112)*/
_AVSyntId_IsGenSymbol=0;
/*SPC(583)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_AVProduction_IsListof=1;
/*SPC(638)*/
_AVProduction_ProdSymbolList=_AVProduction__ProdSymbolauxList;
/*SPC(113)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_51(_TPPrule_51 _currn)
#else
void _VS2rule_51(_currn )
_TPPrule_51 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_51(_TPPrule_51 _currn)
#else
void _VS4rule_51(_currn )
_TPPrule_51 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (AND(GetIsTerm(_currn->_desc1->_ATKey, 0), NOT(GetTermReported(_currn->_desc1->_ATKey, 0)))) {
ResetTermReported(_currn->_desc1->_ATKey, 1);
message(ERROR, CatStrInd("Left-hand side may not be specified TERM: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(792)*/

if (AND(AND(MultipleRoots, GetIsRoot(_currn->_desc1->_ATKey, 0)), NOT(GetRootReported(_currn->_desc1->_ATKey, 0)))) {
ResetRootReported(_currn->_desc1->_ATKey, 1);
message(ERROR, CatStrInd("One of the multiple grammar roots: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(802)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_52(_TPPrule_52 _currn)
#else
void _VS1rule_52(_currn )
_TPPrule_52 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVProduction__ProdSymbolauxList=NULLProdSymbolList;
/*SPC(111)*/
_AVSyntLit_cProdSymbolListPtr_post=_ProdSymbolListADDROF(_AVProduction__ProdSymbolauxList);
/*SPC(112)*/
_AVSyntId_IsGenSymbol=0;
/*SPC(583)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_AVProduction_IsListof=0;
/*SPC(634)*/
_AVProduction_ProdSymbolList=_AVProduction__ProdSymbolauxList;
/*SPC(113)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_52(_TPPrule_52 _currn)
#else
void _VS2rule_52(_currn )
_TPPrule_52 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_52(_TPPrule_52 _currn)
#else
void _VS3rule_52(_currn )
_TPPrule_52 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_52(_TPPrule_52 _currn)
#else
void _VS4rule_52(_currn )
_TPPrule_52 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (AND(GetIsTerm(_currn->_desc1->_ATKey, 0), NOT(GetTermReported(_currn->_desc1->_ATKey, 0)))) {
ResetTermReported(_currn->_desc1->_ATKey, 1);
message(ERROR, CatStrInd("Left-hand side may not be specified TERM: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(770)*/

if (AND(AND(MultipleRoots, GetIsRoot(_currn->_desc1->_ATKey, 0)), NOT(GetRootReported(_currn->_desc1->_ATKey, 0)))) {
ResetRootReported(_currn->_desc1->_ATKey, 1);
message(ERROR, CatStrInd("One of the multiple grammar roots: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(780)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_53(_TPPrule_53 _currn)
#else
void _VS1rule_53(_currn )
_TPPrule_53 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVRuleSpecId_Key=NoKey;
/*SPC(613)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_54(_TPPrule_54 _currn)
#else
void _VS1rule_54(_currn )
_TPPrule_54 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVRuleSpecId_Key=_currn->_desc1->_ATKey;
/*SPC(609)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_54(_TPPrule_54 _currn)
#else
void _VS2rule_54(_currn )
_TPPrule_54 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsSymbol(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as symbol identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(463)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_55(_TPPrule_55 _currn)
#else
void _VS1rule_55(_currn )
_TPPrule_55 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_currn->_ATRuleInstance=MakeRuleProd(_AVRuleSpecId_Key, _AVProduction_ProdSymbolList, _AVProduction_IsListof, (&( _currn->_AT_pos)));
/*SPC(601)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_55(_TPPrule_55 _currn)
#else
void _VS2rule_55(_currn )
_TPPrule_55 _currn;

#endif
{
DefTableKey* _IL_incl8;

_VisitVarDecl()
_VisitEntry();
_IL_incl8=_IG_incl8;_IG_incl8= &(_currn->_ATKey);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_currn->_ATKey=RuleKeyOfRuleProd(_currn->_ATRuleInstance);
/*SPC(627)*/
_AVRuleSpec_Rule=GetRule(_currn->_ATKey, NoRuleProd);
/*SPC(629)*/

if (EQ(GetAttrScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetAttrScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl25)), _currn->_ATKey));

} else {
}
;
/*SPC(834)*/

if (EQ(GetLowerScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetLowerScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));

} else {
}
;
/*SPC(1513)*/
_IG_incl8=_IL_incl8;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_55(_TPPrule_55 _currn)
#else
void _VS3rule_55(_currn )
_TPPrule_55 _currn;

#endif
{
int* _IL_incl6;
DefTableKey* _IL_incl8;

_VisitVarDecl()
_VisitEntry();
_IL_incl6=_IG_incl6;_IG_incl6= &(_currn->_ATIsRule);
_IL_incl8=_IG_incl8;_IG_incl8= &(_currn->_ATKey);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_currn->_ATIsRule=1;
/*SPC(852)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_IG_incl6=_IL_incl6;
_IG_incl8=_IL_incl8;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_55(_TPPrule_55 _currn)
#else
void _VS4rule_55(_currn )
_TPPrule_55 _currn;

#endif
{
int* _IL_incl6;
DefTableKey* _IL_incl8;

_VisitVarDecl()
_VisitEntry();
_IL_incl6=_IG_incl6;_IG_incl6= &(_currn->_ATIsRule);
_IL_incl8=_IG_incl8;_IG_incl8= &(_currn->_ATKey);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_IG_incl6=_IL_incl6;
_IG_incl8=_IL_incl8;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_55(_TPPrule_55 _currn)
#else
void _VS5rule_55(_currn )
_TPPrule_55 _currn;

#endif
{
int* _IL_incl6;
DefTableKey* _IL_incl8;

_VisitVarDecl()
_VisitEntry();
_IL_incl6=_IG_incl6;_IG_incl6= &(_currn->_ATIsRule);
_IL_incl8=_IG_incl8;_IG_incl8= &(_currn->_ATKey);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_AVRuleSpec__const20=_AVCompPart__const20;
/*SPC(599)*/
_IG_incl6=_IL_incl6;
_IG_incl8=_IL_incl8;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_56(_TPPrule_56 _currn)
#else
void _VS1rule_56(_currn )
_TPPrule_56 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSyntId_IsGenSymbol=1;
/*SPC(590)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_56(_TPPrule_56 _currn)
#else
void _VS2rule_56(_currn )
_TPPrule_56 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_56(_TPPrule_56 _currn)
#else
void _VS3rule_56(_currn )
_TPPrule_56 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSyntUnit_newGenTreeAttr_RuleAttr_160=SetCoordSymToBinding(BindIdn(GetAttrScope(_currn->_desc1->_ATKey, NoEnv), GENTREEsym), (&( _currn->_AT_pos)), GENTREEsym);
/*SPC(1044)*/
SetChkAttrClass(KeyOf(_AVSyntUnit_newGenTreeAttr_RuleAttr_160), GENTREEsym, INHClass, (&( _currn->_AT_pos)));
SetChkAttrType(KeyOf(_AVSyntUnit_newGenTreeAttr_RuleAttr_160), GENTREEsym, NODEPTRkey, (&( _currn->_AT_pos)));
;
/*SPC(1050)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_56(_TPPrule_56 _currn)
#else
void _VS4rule_56(_currn )
_TPPrule_56 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsTerm(_currn->_desc1->_ATKey, 0)) {
message(WARNING, CatStrInd("Tree insertion is ignored for a terminal symbol: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(822)*/

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_57(_TPPrule_57 _currn)
#else
void _VS1rule_57(_currn )
_TPPrule_57 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSyntLit_ProdSymbolTakeIt=1;
/*SPC(117)*/
_AVSyntLit_ProdSymbolElem=MakeProdLiteral(_currn->_ATTERM_1);
/*SPC(572)*/
_AVSyntLit_cProdSymbolListPtr_post=
((_AVSyntLit_ProdSymbolTakeIt
) ? (RefEndConsProdSymbolList(_AVSyntLit_cProdSymbolListPtr_post, _AVSyntLit_ProdSymbolElem)
) : (_AVSyntLit_cProdSymbolListPtr_post))
;
/*SPC(118)*/

if (EQ(0, strlen(StringTable(_currn->_ATTERM_1)))) {
message(ERROR, "Literal terminal may not be the empty string", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(578)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_58(_TPPrule_58 _currn)
#else
void _VS1rule_58(_currn )
_TPPrule_58 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_58(_TPPrule_58 _currn)
#else
void _VS2rule_58(_currn )
_TPPrule_58 _currn;

#endif
{
int* _IL_incl31;
int* _IL_incl32;
DefTableKey* _IL_incl30;
int* _IL_incl29;

_VisitVarDecl()
_VisitEntry();
_IL_incl31=_IG_incl31;_IG_incl31= &(_currn->_ATIsCLASSSym);
_IL_incl32=_IG_incl32;_IG_incl32= &(_currn->_ATIsTREESym);
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
_currn->_ATTypeKey=_currn->_desc2->_ATKey;
/*SPC(715)*/
_currn->_ATIsTerm=1;
/*SPC(714)*/
_currn->_ATIsCLASSSym=0;
/*SPC(522)*/
_currn->_ATIsTREESym=1;
/*SPC(521)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_IG_incl31=_IL_incl31;
_IG_incl32=_IL_incl32;
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_58(_TPPrule_58 _currn)
#else
void _VS3rule_58(_currn )
_TPPrule_58 _currn;

#endif
{
int* _IL_incl31;
int* _IL_incl32;
DefTableKey* _IL_incl30;
int* _IL_incl29;

_VisitVarDecl()
_VisitEntry();
_IL_incl31=_IG_incl31;_IG_incl31= &(_currn->_ATIsCLASSSym);
_IL_incl32=_IG_incl32;_IG_incl32= &(_currn->_ATIsTREESym);
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (GetIsSymbol(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as symbol identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(463)*/

if (GetIsRule(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_IG_incl31=_IL_incl31;
_IG_incl32=_IL_incl32;
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_59(_TPPrule_59 _currn)
#else
void _VS1rule_59(_currn )
_TPPrule_59 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc4->_prod])))((NODEPTR) _currn->_desc4);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_59(_TPPrule_59 _currn)
#else
void _VS2rule_59(_currn )
_TPPrule_59 _currn;

#endif
{
DefTableKey* _IL_incl30;
int* _IL_incl29;

_VisitVarDecl()
_VisitEntry();
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
_currn->_ATTypeKey=NoKey;
/*SPC(711)*/
_currn->_ATIsTerm=0;
/*SPC(711)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc4->_prod])))((NODEPTR) _currn->_desc4);
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_59(_TPPrule_59 _currn)
#else
void _VS3rule_59(_currn )
_TPPrule_59 _currn;

#endif
{
DefTableKey* _IL_incl30;
int* _IL_incl29;
Environment* _IL_incl23;
Environment* _IL_incl22;
Environment* _IL_incl21;
Environment* _IL_incl20;
int* _IL_incl6;
DefTableKey* _IL_incl8;

_VisitVarDecl()
_VisitEntry();
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
_IL_incl23=_IG_incl23;_IG_incl23= &(_currn->_ATHEADScope);
_IL_incl22=_IG_incl22;_IG_incl22= &(_currn->_ATUpperScope);
_IL_incl21=_IG_incl21;_IG_incl21= &(_currn->_ATLowerScope);
_IL_incl20=_IG_incl20;_IG_incl20= &(_currn->_ATAttrScope);
_IL_incl6=_IG_incl6;_IG_incl6= &(_currn->_ATIsRule);
_IL_incl8=_IG_incl8;_IG_incl8= &(_currn->_ATKey);

if (GetIsTerm(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("A terminal can not inherit computations: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1186)*/
_currn->_ATAttrScope=GetAttrScope(_currn->_desc2->_ATKey, NoEnv);
/*SPC(1177)*/
_currn->_ATHEADScope=GetHEADScope(_currn->_desc2->_ATKey, NoEnv);
/*SPC(1175)*/
_currn->_ATLowerScope=GetLowerScope(_currn->_desc2->_ATKey, NoEnv);
/*SPC(1173)*/
_currn->_ATUpperScope=GetUpperScope(_currn->_desc2->_ATKey, NoEnv);
/*SPC(1171)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_currn->_ATKey=_currn->_desc2->_ATKey;
/*SPC(857)*/
_currn->_ATIsRule=0;
/*SPC(849)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc4->_prod])))((NODEPTR) _currn->_desc4);
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;
_IG_incl23=_IL_incl23;
_IG_incl22=_IL_incl22;
_IG_incl21=_IL_incl21;
_IG_incl20=_IL_incl20;
_IG_incl6=_IL_incl6;
_IG_incl8=_IL_incl8;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_59(_TPPrule_59 _currn)
#else
void _VS4rule_59(_currn )
_TPPrule_59 _currn;

#endif
{
int* _IL_incl31;
int* _IL_incl32;
DefTableKey* _IL_incl30;
int* _IL_incl29;
Environment* _IL_incl23;
Environment* _IL_incl22;
Environment* _IL_incl21;
Environment* _IL_incl20;
int* _IL_incl6;
DefTableKey* _IL_incl8;

_VisitVarDecl()
_VisitEntry();
_IL_incl31=_IG_incl31;_IG_incl31= &(_currn->_ATIsCLASSSym);
_IL_incl32=_IG_incl32;_IG_incl32= &(_currn->_ATIsTREESym);
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
_IL_incl23=_IG_incl23;_IG_incl23= &(_currn->_ATHEADScope);
_IL_incl22=_IG_incl22;_IG_incl22= &(_currn->_ATUpperScope);
_IL_incl21=_IG_incl21;_IG_incl21= &(_currn->_ATLowerScope);
_IL_incl20=_IG_incl20;_IG_incl20= &(_currn->_ATAttrScope);
_IL_incl6=_IG_incl6;_IG_incl6= &(_currn->_ATIsRule);
_IL_incl8=_IG_incl8;_IG_incl8= &(_currn->_ATKey);
_currn->_ATIsCLASSSym=_currn->_desc1->_ATIsCLASSSym;
/*SPC(518)*/
_currn->_ATIsTREESym=_currn->_desc1->_ATIsTREESym;
/*SPC(517)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc4->_prod])))((NODEPTR) _currn->_desc4);
_IG_incl31=_IL_incl31;
_IG_incl32=_IL_incl32;
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;
_IG_incl23=_IL_incl23;
_IG_incl22=_IL_incl22;
_IG_incl21=_IL_incl21;
_IG_incl20=_IL_incl20;
_IG_incl6=_IL_incl6;
_IG_incl8=_IL_incl8;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_59(_TPPrule_59 _currn)
#else
void _VS5rule_59(_currn )
_TPPrule_59 _currn;

#endif
{
int* _IL_incl31;
int* _IL_incl32;
DefTableKey* _IL_incl30;
int* _IL_incl29;
Environment* _IL_incl23;
Environment* _IL_incl22;
Environment* _IL_incl21;
Environment* _IL_incl20;
int* _IL_incl6;
DefTableKey* _IL_incl8;

_VisitVarDecl()
_VisitEntry();
_IL_incl31=_IG_incl31;_IG_incl31= &(_currn->_ATIsCLASSSym);
_IL_incl32=_IG_incl32;_IG_incl32= &(_currn->_ATIsTREESym);
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
_IL_incl23=_IG_incl23;_IG_incl23= &(_currn->_ATHEADScope);
_IL_incl22=_IG_incl22;_IG_incl22= &(_currn->_ATUpperScope);
_IL_incl21=_IG_incl21;_IG_incl21= &(_currn->_ATLowerScope);
_IL_incl20=_IG_incl20;_IG_incl20= &(_currn->_ATAttrScope);
_IL_incl6=_IG_incl6;_IG_incl6= &(_currn->_ATIsRule);
_IL_incl8=_IG_incl8;_IG_incl8= &(_currn->_ATKey);
_AVSymCompSpec__DefTableKeyauxList_RuleAttr_163=NULLDefTableKeyList;
/*SPC(1064)*/
_AVSymbolDefId_cDefTableKeyListPtr_post=_DefTableKeyListADDROF(_AVSymCompSpec__DefTableKeyauxList_RuleAttr_163);
/*SPC(1065)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

if (GetIsRule(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc4->_prod])))((NODEPTR) _currn->_desc4);
_AVSymCompSpec__const20=_AVCompPart__const20;
/*SPC(515)*/
_AVSymCompSpec_isRoot=GetIsRoot(_currn->_desc2->_ATKey, 0);
/*SPC(1180)*/
_IG_incl31=_IL_incl31;
_IG_incl32=_IL_incl32;
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;
_IG_incl23=_IL_incl23;
_IG_incl22=_IL_incl22;
_IG_incl21=_IL_incl21;
_IG_incl20=_IL_incl20;
_IG_incl6=_IL_incl6;
_IG_incl8=_IL_incl8;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_60(_TPPrule_60 _currn)
#else
void _VS1rule_60(_currn )
_TPPrule_60 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_60(_TPPrule_60 _currn)
#else
void _VS2rule_60(_currn )
_TPPrule_60 _currn;

#endif
{
int* _IL_incl31;
int* _IL_incl32;
DefTableKey* _IL_incl30;
int* _IL_incl29;

_VisitVarDecl()
_VisitEntry();
_IL_incl31=_IG_incl31;_IG_incl31= &(_currn->_ATIsCLASSSym);
_IL_incl32=_IG_incl32;_IG_incl32= &(_currn->_ATIsTREESym);
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
_currn->_ATTypeKey=NoKey;
/*SPC(708)*/
_currn->_ATIsTerm=0;
/*SPC(708)*/
_currn->_ATIsCLASSSym=_currn->_desc1->_ATIsCLASSSym;
/*SPC(513)*/
_currn->_ATIsTREESym=_currn->_desc1->_ATIsTREESym;
/*SPC(512)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_IG_incl31=_IL_incl31;
_IG_incl32=_IL_incl32;
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_60(_TPPrule_60 _currn)
#else
void _VS3rule_60(_currn )
_TPPrule_60 _currn;

#endif
{
DefTableKeyList* _IL_incl38;
int* _IL_incl31;
int* _IL_incl32;
DefTableKey* _IL_incl30;
int* _IL_incl29;

_VisitVarDecl()
_VisitEntry();
_IL_incl38=_IG_incl38;_IG_incl38= &(_currn->_ATSymbolKeyList);
_IL_incl31=_IG_incl31;_IG_incl31= &(_currn->_ATIsCLASSSym);
_IL_incl32=_IG_incl32;_IG_incl32= &(_currn->_ATIsTREESym);
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
_currn->_ATSymbolKeyList=_currn->_desc2->_ATDefTableKeyList;
/*SPC(1003)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_IG_incl38=_IL_incl38;
_IG_incl31=_IL_incl31;
_IG_incl32=_IL_incl32;
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_60(_TPPrule_60 _currn)
#else
void _VS4rule_60(_currn )
_TPPrule_60 _currn;

#endif
{
DefTableKeyList* _IL_incl38;
int* _IL_incl31;
int* _IL_incl32;
DefTableKey* _IL_incl30;
int* _IL_incl29;

_VisitVarDecl()
_VisitEntry();
_IL_incl38=_IG_incl38;_IG_incl38= &(_currn->_ATSymbolKeyList);
_IL_incl31=_IG_incl31;_IG_incl31= &(_currn->_ATIsCLASSSym);
_IL_incl32=_IG_incl32;_IG_incl32= &(_currn->_ATIsTREESym);
_IL_incl30=_IG_incl30;_IG_incl30= &(_currn->_ATTypeKey);
_IL_incl29=_IG_incl29;_IG_incl29= &(_currn->_ATIsTerm);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc3->_prod])))((NODEPTR) _currn->_desc3);
_IG_incl38=_IL_incl38;
_IG_incl31=_IL_incl31;
_IG_incl32=_IL_incl32;
_IG_incl30=_IL_incl30;
_IG_incl29=_IL_incl29;

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_61(_TPPrule_61 _currn)
#else
void _VS1rule_61(_currn )
_TPPrule_61 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATIsCLASSSym=1;
/*SPC(505)*/
_currn->_ATIsTREESym=0;
/*SPC(508)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_62(_TPPrule_62 _currn)
#else
void _VS1rule_62(_currn )
_TPPrule_62 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATIsCLASSSym=0;
/*SPC(508)*/
_currn->_ATIsTREESym=1;
/*SPC(502)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_63(_TPPrule_63 _currn)
#else
void _VS1rule_63(_currn )
_TPPrule_63 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_63(_TPPrule_63 _currn)
#else
void _VS2rule_63(_currn )
_TPPrule_63 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_63(_TPPrule_63 _currn)
#else
void _VS3rule_63(_currn )
_TPPrule_63 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSymbolRef_Key=KeyOf(BindingInEnv((* _IG_incl27), _currn->_desc1->_ATSym));
/*SPC(1325)*/
_AVSymbolRef_ProdSymbol_RuleAttr_167=
((AND((* _IG_incl6), GetIsTerm(_AVSymbolRef_Key, 0))
) ? (FindSymbolRef(GetRule((* _IG_incl8), NoRuleProd), _AVSymbolRef_Key, _currn->_desc2->_ATValue)
) : (NoProdSymbol))
;
/*SPC(1328)*/
_AVSymbolRef_occs_RuleAttr_167=
(((* _IG_incl6)
) ? (CountSymbolRef(GetRule((* _IG_incl8), NoRuleProd), _AVSymbolRef_Key)
) : (0))
;
/*SPC(1342)*/
_AVSymbolRef_Pos=
((NE(_AVSymbolRef_ProdSymbol_RuleAttr_167, NoProdSymbol)
) ? (PosOfProdSymbol(_AVSymbolRef_ProdSymbol_RuleAttr_167)
) : (SUB(0, 1)))
;
/*SPC(1338)*/
_AVSymbolRef_repr=
((EQ(_AVSymbolRef_ProdSymbol_RuleAttr_167, NoProdSymbol)
) ? (newName(_currn->_desc1->_ATSym, (&( _currn->_AT_pos)))
) : (newRuleTermAcc(_AVSymbolRef_Pos, _AVSymbolRef_Key, (&( _currn->_AT_pos)))))
;
/*SPC(1967)*/

if ((* _IG_incl6)) {

if (GetIsNonterm(_AVSymbolRef_Key, 0)) {

if (GT(_AVSymbolRef_occs_RuleAttr_167, 0)) {
message(ERROR, "Attribute of nonterminal is missing", 0, (&( _currn->_AT_pos)));

if (AND(GT(_currn->_desc2->_ATValue, 0), GT(_currn->_desc2->_ATValue, _AVSymbolRef_occs_RuleAttr_167))) {
message(ERROR, "Wrong index of nonterminal occurrence", 0, (&( _currn->_AT_pos)));

} else {
}
;
;

} else {

if (GT(_currn->_desc2->_ATValue, 0)) {
message(ERROR, "Symbol does not occur in rule", 0, (&( _currn->_AT_pos)));

} else {
}
;
}
;

} else {

if (GetIsTerm(_AVSymbolRef_Key, 0)) {

if (AND(GT(_AVSymbolRef_occs_RuleAttr_167, 0), AND(GT(_currn->_desc2->_ATValue, 0), GT(_currn->_desc2->_ATValue, _AVSymbolRef_occs_RuleAttr_167)))) {
message(ERROR, "Wrong index of terminal occurrence", 0, (&( _currn->_AT_pos)));

} else {

if (AND(EQ(_AVSymbolRef_occs_RuleAttr_167, 0), GT(_currn->_desc2->_ATValue, 0))) {
message(ERROR, "Symbol does not occur in rule", 0, (&( _currn->_AT_pos)));

} else {
}
;
}
;

} else {

if (GT(_currn->_desc2->_ATValue, 0)) {
message(ERROR, "Only symbols may be indexed", 0, (&( _currn->_AT_pos)));

} else {
}
;
}
;
}
;

} else {

if (GT(_currn->_desc2->_ATValue, 0)) {
message(ERROR, "Indexed name is not allowed", 0, (&( _currn->_AT_pos)));

} else {
}
;
}
;
/*SPC(1384)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_138(_TPPrule_138 _currn)
#else
void _VS1rule_138(_currn )
_TPPrule_138 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVTypeId_Bind=BindIdn((* _IG_incl27), _currn->_ATSym);
/*SPC(47)*/
_currn->_ATKey=KeyOf(_AVTypeId_Bind);
/*SPC(49)*/
ResetIsType(_currn->_ATKey, 1);
/*SPC(455)*/
ResetNameSym(_currn->_ATKey, _currn->_ATSym);
SetOnceCoord(_currn->_ATKey, (&( _currn->_AT_pos)));
;
/*SPC(417)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_137(_TPPrule_137 _currn)
#else
void _VS1rule_137(_currn )
_TPPrule_137 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_136(_TPPrule_136 _currn)
#else
void _VS1rule_136(_currn )
_TPPrule_136 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSyntId_IsGenSymbol=0;
/*SPC(583)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_136(_TPPrule_136 _currn)
#else
void _VS2rule_136(_currn )
_TPPrule_136 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_136(_TPPrule_136 _currn)
#else
void _VS4rule_136(_currn )
_TPPrule_136 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_135(_TPPrule_135 _currn)
#else
void _VS1rule_135(_currn )
_TPPrule_135 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_135(_TPPrule_135 _currn)
#else
void _VS2rule_135(_currn )
_TPPrule_135 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_135(_TPPrule_135 _currn)
#else
void _VS3rule_135(_currn )
_TPPrule_135 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_135(_TPPrule_135 _currn)
#else
void _VS4rule_135(_currn )
_TPPrule_135 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_133(_TPPrule_133 _currn)
#else
void _VS1rule_133(_currn )
_TPPrule_133 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSyntId_ProdSymbolTakeIt=1;
/*SPC(117)*/
_AVSyntId_Bind=BindIdn((* _IG_incl27), _currn->_ATSym);
/*SPC(47)*/
_currn->_ATKey=KeyOf(_AVSyntId_Bind);
/*SPC(49)*/
ResetNameSym(_currn->_ATKey, _currn->_ATSym);
SetOnceCoord(_currn->_ATKey, (&( _currn->_AT_pos)));
;
/*SPC(417)*/
_AVSyntId_ProdSymbolElem=MakeProdSymbol(_currn->_ATKey, _AVSyntId_IsGenSymbol, (&( _currn->_AT_pos)));
/*SPC(585)*/
_AVSyntLit_cProdSymbolListPtr_post=
((_AVSyntId_ProdSymbolTakeIt
) ? (RefEndConsProdSymbolList(_AVSyntLit_cProdSymbolListPtr_post, _AVSyntId_ProdSymbolElem)
) : (_AVSyntLit_cProdSymbolListPtr_post))
;
/*SPC(118)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_133(_TPPrule_133 _currn)
#else
void _VS2rule_133(_currn )
_TPPrule_133 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (EQ(GetUpperScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetUpperScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
ResetLowerScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
ResetHEADScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
;

} else {
}
;
/*SPC(1148)*/

if (EQ(GetAttrScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetAttrScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl25)), _currn->_ATKey));

} else {
}
;
/*SPC(834)*/
ResetIsDefined(_currn->_ATKey, 1);
/*SPC(1442)*/
ResetIsTreeSym(_currn->_ATKey, 1);
/*SPC(657)*/
ResetIsSymbol(_currn->_ATKey, 1);
/*SPC(451)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_133(_TPPrule_133 _currn)
#else
void _VS3rule_133(_currn )
_TPPrule_133 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (AND(GetIsCLASSSym(_currn->_ATKey, 0), NOT(GetIsClassInRuleReported(_currn->_ATKey, 0)))) {
ResetIsClassInRuleReported(_currn->_ATKey, 1);
message(ERROR, CatStrInd("May not occur in a RULE: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(667)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_132(_TPPrule_132 _currn)
#else
void _VS1rule_132(_currn )
_TPPrule_132 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_132(_TPPrule_132 _currn)
#else
void _VS3rule_132(_currn )
_TPPrule_132 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATKey=
(((* _IG_incl6)
) ? (NoKey
) : ((* _IG_incl8)))
;
/*SPC(1230)*/
_currn->_ATProdSymbol=NoProdSymbol;
/*SPC(1239)*/
_AVSymOcc_AttrClass=NoClass;
/*SPC(1228)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_132(_TPPrule_132 _currn)
#else
void _VS4rule_132(_currn )
_TPPrule_132 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if ((* _IG_incl6)) {
message(ERROR, "Not allowed in RULE computation", 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1233)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_131(_TPPrule_131 _currn)
#else
void _VS1rule_131(_currn )
_TPPrule_131 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATIsCLASSSym=0;
/*SPC(508)*/
_currn->_ATIsTREESym=0;
/*SPC(508)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_130(_TPPrule_130 _currn)
#else
void _VS1rule_130(_currn )
_TPPrule_130 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_ATBind=BindIdn((* _IG_incl27), _currn->_ATSym);
/*SPC(47)*/
_currn->_ATKey=KeyOf(_currn->_ATBind);
/*SPC(49)*/
ResetNameSym(_currn->_ATKey, _currn->_ATSym);
SetOnceCoord(_currn->_ATKey, (&( _currn->_AT_pos)));
;
/*SPC(417)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_130(_TPPrule_130 _currn)
#else
void _VS2rule_130(_currn )
_TPPrule_130 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (EQ(GetUpperScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetUpperScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
ResetLowerScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
ResetHEADScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
;

} else {
}
;
/*SPC(1148)*/

if (EQ(GetAttrScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetAttrScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl25)), _currn->_ATKey));

} else {
}
;
/*SPC(834)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_129(_TPPrule_129 _currn)
#else
void _VS1rule_129(_currn )
_TPPrule_129 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_129(_TPPrule_129 _currn)
#else
void _VS2rule_129(_currn )
_TPPrule_129 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSymbolDefIds__DefTableKeyauxList=NULLDefTableKeyList;
/*SPC(212)*/
_AVSymbolDefId_cDefTableKeyListPtr_post=_DefTableKeyListADDROF(_AVSymbolDefIds__DefTableKeyauxList);
/*SPC(213)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_currn->_ATDefTableKeyList=_AVSymbolDefIds__DefTableKeyauxList;
/*SPC(214)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_129(_TPPrule_129 _currn)
#else
void _VS3rule_129(_currn )
_TPPrule_129 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_128(_TPPrule_128 _currn)
#else
void _VS1rule_128(_currn )
_TPPrule_128 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSymbolDefId_Bind=BindIdn((* _IG_incl27), _currn->_ATSym);
/*SPC(47)*/
_currn->_ATKey=KeyOf(_AVSymbolDefId_Bind);
/*SPC(49)*/
ResetNameSym(_currn->_ATKey, _currn->_ATSym);
SetOnceCoord(_currn->_ATKey, (&( _currn->_AT_pos)));
;
/*SPC(417)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_128(_TPPrule_128 _currn)
#else
void _VS2rule_128(_currn )
_TPPrule_128 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (EQ(GetUpperScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetUpperScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
ResetLowerScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
ResetHEADScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl26)), _currn->_ATKey));
;

} else {
}
;
/*SPC(1148)*/

if (EQ(GetAttrScope(_currn->_ATKey, NoEnv), NoEnv)) {
ResetAttrScope(_currn->_ATKey, AddKeyToEnv(NewScope((* _IG_incl25)), _currn->_ATKey));

} else {
}
;
/*SPC(834)*/

if ((* _IG_incl29)) {
ResetIsTerm(_currn->_ATKey, 1);
SetDiffType(_currn->_ATKey, (* _IG_incl30), ErrorType);
;

} else {
}
;
/*SPC(719)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_128(_TPPrule_128 _currn)
#else
void _VS3rule_128(_currn )
_TPPrule_128 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
ResetIsDefined(_currn->_ATKey, 1);
/*SPC(1439)*/

if ((* _IG_incl32)) {
ResetIsTREESym(_currn->_ATKey, 1);

} else {
}
;

if ((* _IG_incl31)) {
ResetIsCLASSSym(_currn->_ATKey, 1);

} else {
}
;
;
/*SPC(527)*/
ResetIsSymbol(_currn->_ATKey, 1);
/*SPC(435)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_128(_TPPrule_128 _currn)
#else
void _VS4rule_128(_currn )
_TPPrule_128 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSymbolDefId_DefTableKeyTakeIt=1;
/*SPC(218)*/
_AVSymbolDefId_DefTableKeyElem=_currn->_ATKey;
/*SPC(998)*/
_AVSymbolDefId_cDefTableKeyListPtr_post=
((_AVSymbolDefId_DefTableKeyTakeIt
) ? (RefEndConsDefTableKeyList(_AVSymbolDefId_cDefTableKeyListPtr_post, _AVSymbolDefId_DefTableKeyElem)
) : (_AVSymbolDefId_cDefTableKeyListPtr_post))
;
/*SPC(219)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_128(_TPPrule_128 _currn)
#else
void _VS5rule_128(_currn )
_TPPrule_128 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (AND((* _IG_incl29), GetIsNonterm(_currn->_ATKey, 0))) {
message(ERROR, CatStrInd("Terminal occurs on left-hand side of a production: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(758)*/

if (AND((* _IG_incl29), EQ(GetType(_currn->_ATKey, NoKey), ErrorType))) {
message(ERROR, CatStrInd("inconsistent types of terminal: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(738)*/

if (AND(GetIsTreeSym(_currn->_ATKey, 0), AND((* _IG_incl31), NOT(GetIsInRuleReported(_currn->_ATKey, 0))))) {
ResetIsInRuleReported(_currn->_ATKey, 1);
message(ERROR, CatStrInd("May not occur in a RULE: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(691)*/

if (AND(NOT(GetIsTreeSym(_currn->_ATKey, 0)), AND((* _IG_incl32), NOT(GetIsNotInRuleReported(_currn->_ATKey, 0))))) {
ResetIsNotInRuleReported(_currn->_ATKey, 1);
message(ERROR, CatStrInd("Does not occur in a RULE: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(680)*/

if (AND(GetIsTREESym(_currn->_ATKey, 0), AND((* _IG_incl31), NOT(GetIsCLASSReported(_currn->_ATKey, 0))))) {
ResetIsCLASSReported(_currn->_ATKey, 1);
message(ERROR, CatStrInd("Occurs as TREE symbol, too: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(561)*/

if (AND(GetIsCLASSSym(_currn->_ATKey, 0), AND((* _IG_incl32), NOT(GetIsTREEReported(_currn->_ATKey, 0))))) {
ResetIsTREEReported(_currn->_ATKey, 1);
message(ERROR, CatStrInd("Occurs as CLASS symbol, too: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));
;

} else {
}
;
/*SPC(550)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_127(_TPPrule_127 _currn)
#else
void _VS3rule_127(_currn )
_TPPrule_127 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSubtree_SubtreeNo=0;
/*SPC(1806)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_126(_TPPrule_126 _currn)
#else
void _VS5rule_126(_currn )
_TPPrule_126 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVSpec__const20=PTGNull();
/*SPC(2361)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_125(_TPPrule_125 _currn)
#else
void _VS3rule_125(_currn )
_TPPrule_125 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_125(_TPPrule_125 _currn)
#else
void _VS5rule_125(_currn )
_TPPrule_125 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVSpec__const20=_AVSymCompSpec__const20;
/*SPC(2357)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_124(_TPPrule_124 _currn)
#else
void _VS5rule_124(_currn )
_TPPrule_124 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVSpec__const20=PTGNull();
/*SPC(2353)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_123(_TPPrule_123 _currn)
#else
void _VS5rule_123(_currn )
_TPPrule_123 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_AVSpecs__const20=PTGSeq(_AVSpecs__const20, _AVSpec__const20);
/*SPC(2349)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_122(_TPPrule_122 _currn)
#else
void _VS5rule_122(_currn )
_TPPrule_122 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVSpecs__const20=PTGNull();
/*SPC(2345)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_121(_TPPrule_121 _currn)
#else
void _VS5rule_121(_currn )
_TPPrule_121 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVSpec__const20=_AVRuleSpec__const20;
/*SPC(2341)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_120(_TPPrule_120 _currn)
#else
void _VS5rule_120(_currn )
_TPPrule_120 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVSpec__const20=PTGNull();
/*SPC(2337)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_119(_TPPrule_119 _currn)
#else
void _VS5rule_119(_currn )
_TPPrule_119 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVSpec__const20=PTGNull();
/*SPC(2333)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_113(_TPPrule_113 _currn)
#else
void _VS1rule_113(_currn )
_TPPrule_113 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVRuleId_Bind=BindIdn((* _IG_incl27), _currn->_ATSym);
/*SPC(47)*/
_currn->_ATKey=KeyOf(_AVRuleId_Bind);
/*SPC(49)*/
ResetIsRule(_currn->_ATKey, 1);
/*SPC(459)*/
ResetNameSym(_currn->_ATKey, _currn->_ATSym);
SetOnceCoord(_currn->_ATKey, (&( _currn->_AT_pos)));
;
/*SPC(417)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_112(_TPPrule_112 _currn)
#else
void _VS4rule_112(_currn )
_TPPrule_112 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
UnsetRemoteAttr();
/*SPC(1728)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVRemoteClause_RemoteSet=MakeRemoteSet();
/*SPC(1731)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_112(_TPPrule_112 _currn)
#else
void _VS5rule_112(_currn )
_TPPrule_112 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_111(_TPPrule_111 _currn)
#else
void _VS4rule_111(_currn )
_TPPrule_111 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
UnsetRemoteAttr();
/*SPC(1728)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVRemoteClause_RemoteSet=MakeRemoteSet();
/*SPC(1731)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_110(_TPPrule_110 _currn)
#else
void _VS5rule_110(_currn )
_TPPrule_110 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_108(_TPPrule_108 _currn)
#else
void _VS1rule_108(_currn )
_TPPrule_108 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_108(_TPPrule_108 _currn)
#else
void _VS2rule_108(_currn )
_TPPrule_108 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_108(_TPPrule_108 _currn)
#else
void _VS3rule_108(_currn )
_TPPrule_108 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVPlainComp_CompScope=GetLowerScope((* _IG_incl8), NoEnv);
/*SPC(1621)*/
_currn->_ATBind=BindNewPlainComp(_AVPlainComp_CompScope, GetAttrScope((* _IG_incl8), NoEnv), (&( _currn->_AT_pos)));
/*SPC(1625)*/
_currn->_ATBUAssignAttr=
(((* _IG_incl33)
) ? (CreateBUAssignAttr((* _IG_incl8), GetAttribute(KeyOf(_currn->_ATBind), NoKey), (&( _currn->_AT_pos)))
) : (NoKey))
;
/*SPC(1866)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_108(_TPPrule_108 _currn)
#else
void _VS4rule_108(_currn )
_TPPrule_108 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVPlainComp_repr=
(((* _IG_incl33)
) ? (TurnPlainBUIntoAssign((* _IG_incl8), _currn->_ATBUAssignAttr, _AVLoop_repr, (&( _currn->_AT_pos)))
) : (_AVLoop_repr))
;
/*SPC(1875)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_107(_TPPrule_107 _currn)
#else
void _VS1rule_107(_currn )
_TPPrule_107 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_107(_TPPrule_107 _currn)
#else
void _VS2rule_107(_currn )
_TPPrule_107 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_107(_TPPrule_107 _currn)
#else
void _VS3rule_107(_currn )
_TPPrule_107 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVPlainComp_CompScope=GetLowerScope((* _IG_incl8), NoEnv);
/*SPC(1621)*/
_currn->_ATBind=BindNewPlainComp(_AVPlainComp_CompScope, GetAttrScope((* _IG_incl8), NoEnv), (&( _currn->_AT_pos)));
/*SPC(1625)*/
_currn->_ATBUAssignAttr=
(((* _IG_incl33)
) ? (CreateBUAssignAttr((* _IG_incl8), GetAttribute(KeyOf(_currn->_ATBind), NoKey), (&( _currn->_AT_pos)))
) : (NoKey))
;
/*SPC(1866)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_107(_TPPrule_107 _currn)
#else
void _VS4rule_107(_currn )
_TPPrule_107 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1repr)));
_AVPlainComp_repr=
(((* _IG_incl33)
) ? (TurnPlainBUIntoAssign((* _IG_incl8), _currn->_ATBUAssignAttr, _AS1repr, (&( _currn->_AT_pos)))
) : (_AS1repr))
;
/*SPC(1875)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_106(_TPPrule_106 _currn)
#else
void _VS4rule_106(_currn )
_TPPrule_106 _currn;

#endif
{
PExprListPtr _AS1_cPExprListPtr_pre;

_VisitVarDecl()
_VisitEntry();
_AS1_cPExprListPtr_pre=_AVParam_cPExprListPtr_post;
/*SPC(0)*/
(*(_CALL_VS_((NODEPTR ,PExprListPtr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1_cPExprListPtr_pre)));
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_105(_TPPrule_105 _currn)
#else
void _VS4rule_105(_currn )
_TPPrule_105 _currn;

#endif
{
PExprListPtr _AS1_cPExprListPtr_pre;

_VisitVarDecl()
_VisitEntry();
_AS1_cPExprListPtr_pre=_AVParam_cPExprListPtr_post;
/*SPC(0)*/
(*(_CALL_VS_((NODEPTR ,PExprListPtr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1_cPExprListPtr_pre)));

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_104(_TPPrule_104 _currn,PExprList* _AS0_PExprauxList)
#else
void _VS4rule_104(_currn ,_AS0_PExprauxList)
_TPPrule_104 _currn;
PExprList* _AS0_PExprauxList;

#endif
{

_VisitVarDecl()
_VisitEntry();
(* _AS0_PExprauxList)=NULLPExprList;
/*SPC(313)*/
_AVParam_cPExprListPtr_post=_PExprListADDROF((* _AS0_PExprauxList));
/*SPC(314)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVParamsOpt_PExprList=(* _AS0_PExprauxList);
/*SPC(315)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_103(_TPPrule_103 _currn,PExprList* _AS0_PExprauxList)
#else
void _VS4rule_103(_currn ,_AS0_PExprauxList)
_TPPrule_103 _currn;
PExprList* _AS0_PExprauxList;

#endif
{

_VisitVarDecl()
_VisitEntry();
(* _AS0_PExprauxList)=NULLPExprList;
/*SPC(313)*/
_AVParamsOpt_HEAD$47_RuleAttr_203=_PExprListADDROF((* _AS0_PExprauxList));
/*SPC(314)*/
_AVParamsOpt_PExprList=(* _AS0_PExprauxList);
/*SPC(315)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_102(_TPPrule_102 _currn,PExprListPtr* _AS0_cPExprListPtr_pre)
#else
void _VS4rule_102(_currn ,_AS0_cPExprListPtr_pre)
_TPPrule_102 _currn;
PExprListPtr* _AS0_cPExprListPtr_pre;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1repr)));
_AVParam_PExprTakeIt=1;
/*SPC(319)*/
_AVParam_PExprElem=_AS1repr;
/*SPC(1923)*/
_AVParam_cPExprListPtr_post=
((_AVParam_PExprTakeIt
) ? (RefEndConsPExprList((* _AS0_cPExprListPtr_pre), _AVParam_PExprElem)
) : ((* _AS0_cPExprListPtr_pre)))
;
/*SPC(320)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_97(_TPPrule_97 _currn)
#else
void _VS1rule_97(_currn )
_TPPrule_97 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_97(_TPPrule_97 _currn)
#else
void _VS2rule_97(_currn )
_TPPrule_97 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_97(_TPPrule_97 _currn)
#else
void _VS3rule_97(_currn )
_TPPrule_97 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (GetIsRule(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc1->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc1->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_96(_TPPrule_96 _currn)
#else
void _VS1rule_96(_currn )
_TPPrule_96 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_96(_TPPrule_96 _currn)
#else
void _VS2rule_96(_currn )
_TPPrule_96 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_96(_TPPrule_96 _currn)
#else
void _VS3rule_96(_currn )
_TPPrule_96 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

if (GetIsRule(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as rule identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(483)*/

if (GetIsType(_currn->_desc2->_ATKey, 0)) {
message(ERROR, CatStrInd("Used as type identifier elsewhere: ", _currn->_desc2->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(473)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_93(_TPPrule_93 _currn)
#else
void _VS4rule_93(_currn )
_TPPrule_93 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVDepClause__PExprauxList=NULLPExprList;
/*SPC(313)*/
_AVParam_cPExprListPtr_post=_PExprListADDROF(_AVDepClause__PExprauxList);
/*SPC(314)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVDepClause_PExprList=_AVDepClause__PExprauxList;
/*SPC(315)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_92(_TPPrule_92 _currn)
#else
void _VS4rule_92(_currn )
_TPPrule_92 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVDepClause__PExprauxList=NULLPExprList;
/*SPC(313)*/
_AVParam_cPExprListPtr_post=_PExprListADDROF(_AVDepClause__PExprauxList);
/*SPC(314)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVDepClause_PExprList=_AVDepClause__PExprauxList;
/*SPC(315)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_89(_TPPrule_89 _currn)
#else
void _VS4rule_89(_currn )
_TPPrule_89 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVDepAttr_PExprTakeIt=1;
/*SPC(319)*/
_AVDepAttr_PExprElem=_currn->_desc1->_ATrepr;
/*SPC(1913)*/
_AVParam_cPExprListPtr_post=
((_AVDepAttr_PExprTakeIt
) ? (RefEndConsPExprList(_AVParam_cPExprListPtr_post, _AVDepAttr_PExprElem)
) : (_AVParam_cPExprListPtr_post))
;
/*SPC(320)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_88(_TPPrule_88 _currn)
#else
void _VS4rule_88(_currn )
_TPPrule_88 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVDepAttr_PExprTakeIt=1;
/*SPC(319)*/
_AVDepAttr_PExprElem=_AVRemoteExpression_repr;
/*SPC(1913)*/
_AVParam_cPExprListPtr_post=
((_AVDepAttr_PExprTakeIt
) ? (RefEndConsPExprList(_AVParam_cPExprListPtr_post, _AVDepAttr_PExprElem)
) : (_AVParam_cPExprListPtr_post))
;
/*SPC(320)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_87(_TPPrule_87 _currn)
#else
void _VS3rule_87(_currn )
_TPPrule_87 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
_currn->_desc1->_ATIsDefining=0;
/*SPC(1405)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_87(_TPPrule_87 _currn)
#else
void _VS4rule_87(_currn )
_TPPrule_87 _currn;

#endif
{
PExpr _AS1repr;

_VisitVarDecl()
_VisitEntry();
_AVAttr_IsIterate=0;
/*SPC(985)*/
(*(_CALL_VS_((NODEPTR ,PExpr*)) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1repr)));
_AVDepAttr_PExprTakeIt=1;
/*SPC(319)*/
_AVDepAttr_PExprElem=_AS1repr;
/*SPC(1913)*/
_AVParam_cPExprListPtr_post=
((_AVDepAttr_PExprTakeIt
) ? (RefEndConsPExprList(_AVParam_cPExprListPtr_post, _AVDepAttr_PExprElem)
) : (_AVParam_cPExprListPtr_post))
;
/*SPC(320)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_86(_TPPrule_86 _currn)
#else
void _VS5rule_86(_currn )
_TPPrule_86 _currn;

#endif
{
PTGNode _AS1_const20;

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR ,PTGNode*)) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1,(&( _AS1_const20)));
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_AVComputations__const20=PTGSeq(_AS1_const20, _AVComputations__const20);
/*SPC(2201)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_85(_TPPrule_85 _currn)
#else
void _VS5rule_85(_currn )
_TPPrule_85 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVComputations__const20=PTGNull();
/*SPC(2197)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_84(_TPPrule_84 _currn)
#else
void _VS3rule_84(_currn )
_TPPrule_84 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_currn->_desc1->_ATIsBottomUp=0;
/*SPC(1675)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_84(_TPPrule_84 _currn,PTGNode* _AS0_const20)
#else
void _VS5rule_84(_currn ,_AS0_const20)
_TPPrule_84 _currn;
PTGNode* _AS0_const20;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(* _AS0_const20)=IDENTICAL(_AVCompute_InhComps);
/*SPC(2193)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_83(_TPPrule_83 _currn)
#else
void _VS5rule_83(_currn )
_TPPrule_83 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVCompPart__const20=_AVComputations__const20;
/*SPC(2189)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_82(_TPPrule_82 _currn)
#else
void _VS5rule_82(_currn )
_TPPrule_82 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVCompPart__const20=PTGNull();
/*SPC(2185)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_79(_TPPrule_79 _currn)
#else
void _VS1rule_79(_currn )
_TPPrule_79 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVChainName_Bind=BindIdn((* _IG_incl18), _currn->_ATSym);
/*SPC(895)*/
_currn->_ATKey=KeyOf(_AVChainName_Bind);
/*SPC(49)*/
ResetIsChain(_currn->_ATKey, 1);
/*SPC(896)*/
ResetNameSym(_currn->_ATKey, _currn->_ATSym);
SetOnceCoord(_currn->_ATKey, (&( _currn->_AT_pos)));
;
/*SPC(417)*/

if (IsPredefSym(_currn->_ATSym)) {
message(ERROR, CatStrInd("Predefined name is not allowed: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1506)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_79(_TPPrule_79 _currn)
#else
void _VS2rule_79(_currn )
_TPPrule_79 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (NE(BindingInScope((* _IG_incl34), _currn->_ATSym), NoBinding)) {
message(ERROR, CatStrInd("There is an ATTR definition elsewhere: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1118)*/
SetChkAttrType(_currn->_ATKey, _currn->_ATSym, (* _IG_incl35), (&( _currn->_AT_pos)));
/*SPC(1109)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_76(_TPPrule_76 _currn)
#else
void _VS1rule_76(_currn )
_TPPrule_76 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVAttrName_Bind=SetCoordSymToBinding(BindIdn((* _IG_incl34), _currn->_ATSym), (&( _currn->_AT_pos)), _currn->_ATSym);
/*SPC(1083)*/
_currn->_ATKey=KeyOf(_AVAttrName_Bind);
/*SPC(49)*/
ResetNameSym(_currn->_ATKey, _currn->_ATSym);
SetOnceCoord(_currn->_ATKey, (&( _currn->_AT_pos)));
;
/*SPC(417)*/

if (IsPredefSym(_currn->_ATSym)) {
message(ERROR, CatStrInd("Predefined name is not allowed: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1506)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_76(_TPPrule_76 _currn)
#else
void _VS2rule_76(_currn )
_TPPrule_76 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();

if (NE(BindingInScope((* _IG_incl18), _currn->_ATSym), NoBinding)) {
message(ERROR, CatStrInd("There is a CHAIN definition elsewhere: ", _currn->_ATSym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1100)*/
SetChkAttrClass(_currn->_ATKey, _currn->_ATSym, (* _IG_incl36), (&( _currn->_AT_pos)));
SetChkAttrType(_currn->_ATKey, _currn->_ATSym, (* _IG_incl37), (&( _currn->_AT_pos)));
;
/*SPC(1088)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_69(_TPPrule_69 _currn)
#else
void _VS1rule_69(_currn )
_TPPrule_69 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVAttrDefId_Sym=_currn->_ATTERM_1;
/*SPC(414)*/
BindDefAttrs(_AVAttrDefId_Sym, (* _IG_incl38), (* _IG_incl18), (* _IG_incl39), (* _IG_incl40), (&( _currn->_AT_pos)));
/*SPC(1031)*/

if (IsPredefSym(_AVAttrDefId_Sym)) {
message(ERROR, CatStrInd("Predefined name is not allowed: ", _AVAttrDefId_Sym), 0, (&( _currn->_AT_pos)));

} else {
}
;
/*SPC(1506)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_68(_TPPrule_68 _currn)
#else
void _VS1rule_68(_currn )
_TPPrule_68 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS2rule_68(_TPPrule_68 _currn)
#else
void _VS2rule_68(_currn )
_TPPrule_68 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS3rule_68(_TPPrule_68 _currn)
#else
void _VS3rule_68(_currn )
_TPPrule_68 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS4rule_68(_TPPrule_68 _currn)
#else
void _VS4rule_68(_currn )
_TPPrule_68 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
_AVAttrComp__const3=_currn->_desc1->_ATBind;
/*SPC(2129)*/
_currn->_AT_const4=_AVDefAttr_IsUpperSymbComp;
/*SPC(2129)*/
_currn->_ATisAccu=0;
/*SPC(2110)*/
_AVAttrComp_IsChainStart=NoKey;
/*SPC(1654)*/
ResetHasNonAccuAsgn(_currn->_desc1->_ATAttrKey, 1);
/*SPC(2103)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS5rule_68(_TPPrule_68 _currn)
#else
void _VS5rule_68(_currn )
_TPPrule_68 _currn;

#endif
{

_VisitVarDecl()
_VisitEntry();
_AVDefAttr_hasAccuToken=0;
/*SPC(2062)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc2->_prod])))((NODEPTR) _currn->_desc2);
_AVAttrComp_repr=newAssign(_AVDefAttr_repr, _AVLoop_repr, (&( _currn->_AT_pos)));
/*SPC(1856)*/

_VisitExit();
}

#if defined(__STDC__) || defined(__cplusplus)
void _VS1rule_64(_TPPrule_64 _currn)
#else
void _VS1rule_64(_currn )
_TPPrule_64 _currn;

#endif
{
Environment* _IL_incl34;
Environment* _IL_incl27;
Environment* _IL_incl26;
Environment* _IL_incl25;
Environment* _IL_incl18;

_VisitVarDecl()
_VisitEntry();
_IL_incl34=_IG_incl34;_IG_incl34= &(_currn->_ATAttrNameEnv);
_IL_incl27=_IG_incl27;_IG_incl27= &(_currn->_ATEnv);
_IL_incl26=_IG_incl26;_IG_incl26= &(_currn->_ATSymbolEnv);
_IL_incl25=_IG_incl25;_IG_incl25= &(_currn->_ATAttrEnv);
_IL_incl18=_IG_incl18;_IG_incl18= &(_currn->_ATChainScope);
_currn->_ATSymbolEnv=NewEnv();
/*SPC(1492)*/
_currn->_ATAttrNameEnv=NewEnv();
/*SPC(1491)*/
_currn->_ATChainScope=NewScope(_currn->_ATAttrNameEnv);
/*SPC(889)*/
_currn->_ATAttrEnv=NewEnv();
/*SPC(1490)*/
_currn->_ATEnv=
(MakePredef(RootEnv, _currn->_ATAttrEnv, _currn->_ATAttrNameEnv, _currn->_ATSymbolEnv), RootEnv)
;
/*SPC(1493)*/
(*(_CALL_VS_((NODEPTR )) (VS1MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
MakeGrammar(_currn->_ATEnv);
/*SPC(618)*/
(*(_CALL_VS_((NODEPTR )) (VS2MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
PTGOutFile("ligaprod", OutputRules());
/*SPC(644)*/
TransformListofRules(_currn->_ATEnv);
ClassifySymbols();
;
/*SPC(743)*/
MakeSymbOccScopes(_currn->_ATSymbolEnv);
/*SPC(1142)*/
(*(_CALL_VS_((NODEPTR )) (VS3MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
MarkGrammarUses(_currn->_ATEnv);
/*SPC(1474)*/
InheritAttributes(_currn->_ATEnv, _currn->_ATAttrNameEnv);
/*SPC(1130)*/
PTGOutFile(CatStrStr(SRCFILE, ".symb"), OutputSymbols(_currn->_ATEnv));
/*SPC(1482)*/
(*(_CALL_VS_((NODEPTR )) (VS4MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
InheritCompute(_currn->_ATChainScope);
/*SPC(1678)*/
PropagateAllAttrsInhAccu(_currn->_ATEnv);
/*SPC(2042)*/
(*(_CALL_VS_((NODEPTR )) (VS5MAP[_currn->_desc1->_prod])))((NODEPTR) _currn->_desc1);
PTGOutFile(CatStrStr(SRCFILE, ".inhcmp"), PTGInhComputations(_AVSpecs__const20));
/*SPC(1690)*/
AccumulateInherit(_currn->_ATEnv);
/*SPC(2047)*/
InheritRepr();
/*SPC(1838)*/
CheckRuleComps();
/*SPC(1841)*/
PTGOutFile(CatStrStr(SRCFILE, ".remote"), RemoteOutput());
/*SPC(1771)*/
_AVAG_TargetDefs=OutputDefs(_currn->_ATEnv, _currn->_ATChainScope);
/*SPC(2015)*/
PTGOutFile(CatStrStr(SRCFILE, ".comp"), ReprOut());
/*SPC(2003)*/
_AVAG_TargetComps=CompOut();
/*SPC(2027)*/
PTGOutFile("liga.out", PTGIdlAttrEval(_AVAG_TargetComps, _AVAG_TargetDefs));
/*SPC(2033)*/
_IG_incl34=_IL_incl34;
_IG_incl27=_IL_incl27;
_IG_incl26=_IL_incl26;
_IG_incl25=_IL_incl25;
_IG_incl18=_IL_incl18;

_VisitExit();
}

