#ifndef PDL_GEN_H
#define PDL_GEN_H

#include "deftbl.h"

#ifdef ELI_ARG
#undef ELI_ARG
#endif

#if defined(__STDC__) || defined(__cplusplus)
#define ELI_ARG(proto)    proto
#else
#define ELI_ARG(proto)    ()
#endif

#include "CoordCmp.h"
#include "err.h"
#include "RuleProd.h"
#include "DefTableKeyList.h"
#include "envmod.h"
#include "err.h"
#include "envmod.h"
#include "envmod.h"
#include "DefTableKeyList.h"
#include "DefTableKeyList.h"
#include "SymbAttrList.h"
#include "DefTableKeyList.h"
#include "DefTableKeyList.h"
#include "ExprRepr.h"
#include "DefTableKeyList.h"
#include "ExprRepr.h"
#include "PExprList.h"
#include "PExprList.h"

#define TYPE int
extern TYPE Getint ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void Setint ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void Resetint ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE
#define TYPE CoordPtr
extern TYPE GetCoordPtr ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetCoordPtr ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetCoordPtr ELI_ARG((int _Property, DefTableKey key, TYPE _val));
extern TYPE SetOnceCoordPtr ELI_ARG((int _Property, DefTableKey key, TYPE val));
#undef TYPE
#define TYPE RuleProd
extern TYPE GetRuleProd ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetRuleProd ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetRuleProd ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE
#define TYPE DefTableKey
extern TYPE GetDefTableKey ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetDefTableKey ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetDefTableKey ELI_ARG((int _Property, DefTableKey key, TYPE _val));
extern void SetDiffDefTableKey ELI_ARG((int _Property, DefTableKey key, TYPE thistype, TYPE diff));
#undef TYPE
#define TYPE DefTableKeyList
extern TYPE GetDefTableKeyList ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetDefTableKeyList ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetDefTableKeyList ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE
#define TYPE Environment
extern TYPE GetEnvironment ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetEnvironment ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetEnvironment ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE
#define TYPE Binding
extern TYPE GetBinding ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetBinding ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetBinding ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE
#define TYPE SymbAttrList
extern TYPE GetSymbAttrList ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetSymbAttrList ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetSymbAttrList ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE
#define TYPE PExpr
extern TYPE GetPExpr ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetPExpr ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetPExpr ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE
#define TYPE PExprList
extern TYPE GetPExprList ELI_ARG((int _Property, DefTableKey key, TYPE deflt));
extern void SetPExprList ELI_ARG((int _Property, DefTableKey key, TYPE _add, TYPE _replace));
extern void ResetPExprList ELI_ARG((int _Property, DefTableKey key, TYPE _val));
#undef TYPE

#define SetClpValue(key, _add, _replace)   \
		Setint(1, (key), (_add), (_replace))
#define ResetClpValue(key, _val)   \
		Resetint(1, (key), (_val))
#define GetClpValue(key, deflt)   \
		Getint(1, (key), (deflt))
#define SetIsSymbol(key, _add, _replace)   \
		Setint(2, (key), (_add), (_replace))
#define ResetIsSymbol(key, _val)   \
		Resetint(2, (key), (_val))
#define GetIsSymbol(key, deflt)   \
		Getint(2, (key), (deflt))
#define SetIsType(key, _add, _replace)   \
		Setint(3, (key), (_add), (_replace))
#define ResetIsType(key, _val)   \
		Resetint(3, (key), (_val))
#define GetIsType(key, deflt)   \
		Getint(3, (key), (deflt))
#define SetIsRule(key, _add, _replace)   \
		Setint(4, (key), (_add), (_replace))
#define ResetIsRule(key, _val)   \
		Resetint(4, (key), (_val))
#define GetIsRule(key, deflt)   \
		Getint(4, (key), (deflt))
#define SetNameSym(key, _add, _replace)   \
		Setint(5, (key), (_add), (_replace))
#define ResetNameSym(key, _val)   \
		Resetint(5, (key), (_val))
#define GetNameSym(key, deflt)   \
		Getint(5, (key), (deflt))
#define SetOnceCoord(key, val)   \
		SetOnceCoordPtr(6, (key), (val))
#define SetCoord(key, _add, _replace)   \
		SetCoordPtr(6, (key), (_add), (_replace))
#define ResetCoord(key, _val)   \
		ResetCoordPtr(6, (key), (_val))
#define GetCoord(key, deflt)   \
		GetCoordPtr(6, (key), (deflt))
#define SetIsTREESym(key, _add, _replace)   \
		Setint(7, (key), (_add), (_replace))
#define ResetIsTREESym(key, _val)   \
		Resetint(7, (key), (_val))
#define GetIsTREESym(key, deflt)   \
		Getint(7, (key), (deflt))
#define SetIsCLASSSym(key, _add, _replace)   \
		Setint(8, (key), (_add), (_replace))
#define ResetIsCLASSSym(key, _val)   \
		Resetint(8, (key), (_val))
#define GetIsCLASSSym(key, deflt)   \
		Getint(8, (key), (deflt))
#define SetIsTREEReported(key, _add, _replace)   \
		Setint(9, (key), (_add), (_replace))
#define ResetIsTREEReported(key, _val)   \
		Resetint(9, (key), (_val))
#define GetIsTREEReported(key, deflt)   \
		Getint(9, (key), (deflt))
#define SetIsCLASSReported(key, _add, _replace)   \
		Setint(10, (key), (_add), (_replace))
#define ResetIsCLASSReported(key, _val)   \
		Resetint(10, (key), (_val))
#define GetIsCLASSReported(key, deflt)   \
		Getint(10, (key), (deflt))
#define SetIsNonterm(key, _add, _replace)   \
		Setint(11, (key), (_add), (_replace))
#define ResetIsNonterm(key, _val)   \
		Resetint(11, (key), (_val))
#define GetIsNonterm(key, deflt)   \
		Getint(11, (key), (deflt))
#define SetHasListof(key, _add, _replace)   \
		Setint(12, (key), (_add), (_replace))
#define ResetHasListof(key, _val)   \
		Resetint(12, (key), (_val))
#define GetHasListof(key, deflt)   \
		Getint(12, (key), (deflt))
#define SetHasNonListof(key, _add, _replace)   \
		Setint(13, (key), (_add), (_replace))
#define ResetHasNonListof(key, _val)   \
		Resetint(13, (key), (_val))
#define GetHasNonListof(key, deflt)   \
		Getint(13, (key), (deflt))
#define SetRule(key, _add, _replace)   \
		SetRuleProd(14, (key), (_add), (_replace))
#define ResetRule(key, _val)   \
		ResetRuleProd(14, (key), (_val))
#define GetRule(key, deflt)   \
		GetRuleProd(14, (key), (deflt))
#define SetLhsOfRule(key, _add, _replace)   \
		SetDefTableKey(15, (key), (_add), (_replace))
#define ResetLhsOfRule(key, _val)   \
		ResetDefTableKey(15, (key), (_val))
#define GetLhsOfRule(key, deflt)   \
		GetDefTableKey(15, (key), (deflt))
#define SetListofRuleOfLhs(key, _add, _replace)   \
		SetDefTableKey(16, (key), (_add), (_replace))
#define ResetListofRuleOfLhs(key, _val)   \
		ResetDefTableKey(16, (key), (_val))
#define GetListofRuleOfLhs(key, deflt)   \
		GetDefTableKey(16, (key), (deflt))
#define SetDiffType(key, thistype, diff)   \
		SetDiffDefTableKey(17, (key), (thistype), (diff))
#define SetType(key, _add, _replace)   \
		SetDefTableKey(17, (key), (_add), (_replace))
#define ResetType(key, _val)   \
		ResetDefTableKey(17, (key), (_val))
#define GetType(key, deflt)   \
		GetDefTableKey(17, (key), (deflt))
#define SetIsTerm(key, _add, _replace)   \
		Setint(18, (key), (_add), (_replace))
#define ResetIsTerm(key, _val)   \
		Resetint(18, (key), (_val))
#define GetIsTerm(key, deflt)   \
		Getint(18, (key), (deflt))
#define SetIsRoot(key, _add, _replace)   \
		Setint(19, (key), (_add), (_replace))
#define ResetIsRoot(key, _val)   \
		Resetint(19, (key), (_val))
#define GetIsRoot(key, deflt)   \
		Getint(19, (key), (deflt))
#define SetTermReported(key, _add, _replace)   \
		Setint(20, (key), (_add), (_replace))
#define ResetTermReported(key, _val)   \
		Resetint(20, (key), (_val))
#define GetTermReported(key, deflt)   \
		Getint(20, (key), (deflt))
#define SetRootReported(key, _add, _replace)   \
		Setint(21, (key), (_add), (_replace))
#define ResetRootReported(key, _val)   \
		Resetint(21, (key), (_val))
#define GetRootReported(key, deflt)   \
		Getint(21, (key), (deflt))
#define SetIsTreeSym(key, _add, _replace)   \
		Setint(22, (key), (_add), (_replace))
#define ResetIsTreeSym(key, _val)   \
		Resetint(22, (key), (_val))
#define GetIsTreeSym(key, deflt)   \
		Getint(22, (key), (deflt))
#define SetIsNotInRuleReported(key, _add, _replace)   \
		Setint(23, (key), (_add), (_replace))
#define ResetIsNotInRuleReported(key, _val)   \
		Resetint(23, (key), (_val))
#define GetIsNotInRuleReported(key, deflt)   \
		Getint(23, (key), (deflt))
#define SetIsInRuleReported(key, _add, _replace)   \
		Setint(24, (key), (_add), (_replace))
#define ResetIsInRuleReported(key, _val)   \
		Resetint(24, (key), (_val))
#define GetIsInRuleReported(key, deflt)   \
		Getint(24, (key), (deflt))
#define SetIsClassInRuleReported(key, _add, _replace)   \
		Setint(25, (key), (_add), (_replace))
#define ResetIsClassInRuleReported(key, _val)   \
		Resetint(25, (key), (_val))
#define GetIsClassInRuleReported(key, deflt)   \
		Getint(25, (key), (deflt))
#define SetDerivableFrom(key, _add, _replace)   \
		SetDefTableKeyList(26, (key), (_add), (_replace))
#define ResetDerivableFrom(key, _val)   \
		ResetDefTableKeyList(26, (key), (_val))
#define GetDerivableFrom(key, deflt)   \
		GetDefTableKeyList(26, (key), (deflt))
#define SetAttrScope(key, _add, _replace)   \
		SetEnvironment(27, (key), (_add), (_replace))
#define ResetAttrScope(key, _val)   \
		ResetEnvironment(27, (key), (_val))
#define GetAttrScope(key, deflt)   \
		GetEnvironment(27, (key), (deflt))
#define SetIsDefined(key, _add, _replace)   \
		Setint(28, (key), (_add), (_replace))
#define ResetIsDefined(key, _val)   \
		Resetint(28, (key), (_val))
#define GetIsDefined(key, deflt)   \
		Getint(28, (key), (deflt))
#define SetAttrClass(key, _add, _replace)   \
		Setint(29, (key), (_add), (_replace))
#define ResetAttrClass(key, _val)   \
		Resetint(29, (key), (_val))
#define GetAttrClass(key, deflt)   \
		Getint(29, (key), (deflt))
#define SetAttrClassCoord(key, _add, _replace)   \
		SetCoordPtr(30, (key), (_add), (_replace))
#define ResetAttrClassCoord(key, _val)   \
		ResetCoordPtr(30, (key), (_val))
#define GetAttrClassCoord(key, deflt)   \
		GetCoordPtr(30, (key), (deflt))
#define SetAttrClassReported(key, _add, _replace)   \
		Setint(31, (key), (_add), (_replace))
#define ResetAttrClassReported(key, _val)   \
		Resetint(31, (key), (_val))
#define GetAttrClassReported(key, deflt)   \
		Getint(31, (key), (deflt))
#define SetAttrClassDone(key, _add, _replace)   \
		Setint(32, (key), (_add), (_replace))
#define ResetAttrClassDone(key, _val)   \
		Resetint(32, (key), (_val))
#define GetAttrClassDone(key, deflt)   \
		Getint(32, (key), (deflt))
#define SetAttrType(key, _add, _replace)   \
		SetDefTableKey(33, (key), (_add), (_replace))
#define ResetAttrType(key, _val)   \
		ResetDefTableKey(33, (key), (_val))
#define GetAttrType(key, deflt)   \
		GetDefTableKey(33, (key), (deflt))
#define SetAttrTypeCoord(key, _add, _replace)   \
		SetCoordPtr(34, (key), (_add), (_replace))
#define ResetAttrTypeCoord(key, _val)   \
		ResetCoordPtr(34, (key), (_val))
#define GetAttrTypeCoord(key, deflt)   \
		GetCoordPtr(34, (key), (deflt))
#define SetAttrTypeReported(key, _add, _replace)   \
		Setint(35, (key), (_add), (_replace))
#define ResetAttrTypeReported(key, _val)   \
		Resetint(35, (key), (_val))
#define GetAttrTypeReported(key, deflt)   \
		Getint(35, (key), (deflt))
#define SetAttrTypeDone(key, _add, _replace)   \
		Setint(36, (key), (_add), (_replace))
#define ResetAttrTypeDone(key, _val)   \
		Resetint(36, (key), (_val))
#define GetAttrTypeDone(key, deflt)   \
		Getint(36, (key), (deflt))
#define SetAttrTypeDefault(key, _add, _replace)   \
		Setint(37, (key), (_add), (_replace))
#define ResetAttrTypeDefault(key, _val)   \
		Resetint(37, (key), (_val))
#define GetAttrTypeDefault(key, deflt)   \
		Getint(37, (key), (deflt))
#define SetIsChain(key, _add, _replace)   \
		Setint(38, (key), (_add), (_replace))
#define ResetIsChain(key, _val)   \
		Resetint(38, (key), (_val))
#define GetIsChain(key, deflt)   \
		Getint(38, (key), (deflt))
#define SetUpperScope(key, _add, _replace)   \
		SetEnvironment(39, (key), (_add), (_replace))
#define ResetUpperScope(key, _val)   \
		ResetEnvironment(39, (key), (_val))
#define GetUpperScope(key, deflt)   \
		GetEnvironment(39, (key), (deflt))
#define SetLowerScope(key, _add, _replace)   \
		SetEnvironment(40, (key), (_add), (_replace))
#define ResetLowerScope(key, _val)   \
		ResetEnvironment(40, (key), (_val))
#define GetLowerScope(key, deflt)   \
		GetEnvironment(40, (key), (deflt))
#define SetHEADScope(key, _add, _replace)   \
		SetEnvironment(41, (key), (_add), (_replace))
#define ResetHEADScope(key, _val)   \
		ResetEnvironment(41, (key), (_val))
#define GetHEADScope(key, deflt)   \
		GetEnvironment(41, (key), (deflt))
#define SetIsDefinedReported(key, _add, _replace)   \
		Setint(42, (key), (_add), (_replace))
#define ResetIsDefinedReported(key, _val)   \
		Resetint(42, (key), (_val))
#define GetIsDefinedReported(key, deflt)   \
		Getint(42, (key), (deflt))
#define SetIsUsed(key, _add, _replace)   \
		Setint(43, (key), (_add), (_replace))
#define ResetIsUsed(key, _val)   \
		Resetint(43, (key), (_val))
#define GetIsUsed(key, deflt)   \
		Getint(43, (key), (deflt))
#define SetIsMarked(key, _add, _replace)   \
		Setint(44, (key), (_add), (_replace))
#define ResetIsMarked(key, _val)   \
		Resetint(44, (key), (_val))
#define GetIsMarked(key, deflt)   \
		Getint(44, (key), (deflt))
#define SetCheckMult(key, _add, _replace)   \
		Setint(45, (key), (_add), (_replace))
#define ResetCheckMult(key, _val)   \
		Resetint(45, (key), (_val))
#define GetCheckMult(key, deflt)   \
		Getint(45, (key), (deflt))
#define SetInheritedFrom(key, _add, _replace)   \
		SetBinding(46, (key), (_add), (_replace))
#define ResetInheritedFrom(key, _val)   \
		ResetBinding(46, (key), (_val))
#define GetInheritedFrom(key, deflt)   \
		GetBinding(46, (key), (deflt))
#define SetInheritedBy(key, _add, _replace)   \
		SetDefTableKeyList(47, (key), (_add), (_replace))
#define ResetInheritedBy(key, _val)   \
		ResetDefTableKeyList(47, (key), (_val))
#define GetInheritedBy(key, deflt)   \
		GetDefTableKeyList(47, (key), (deflt))
#define SetAttribute(key, _add, _replace)   \
		SetDefTableKey(48, (key), (_add), (_replace))
#define ResetAttribute(key, _val)   \
		ResetDefTableKey(48, (key), (_val))
#define GetAttribute(key, deflt)   \
		GetDefTableKey(48, (key), (deflt))
#define SetContext(key, _add, _replace)   \
		SetDefTableKey(49, (key), (_add), (_replace))
#define ResetContext(key, _val)   \
		ResetDefTableKey(49, (key), (_val))
#define GetContext(key, deflt)   \
		GetDefTableKey(49, (key), (deflt))
#define SetIsRuleComputation(key, _add, _replace)   \
		Setint(50, (key), (_add), (_replace))
#define ResetIsRuleComputation(key, _val)   \
		Resetint(50, (key), (_val))
#define GetIsRuleComputation(key, deflt)   \
		Getint(50, (key), (deflt))
#define SetIsBottomUp(key, _add, _replace)   \
		Setint(51, (key), (_add), (_replace))
#define ResetIsBottomUp(key, _val)   \
		Resetint(51, (key), (_val))
#define GetIsBottomUp(key, deflt)   \
		Getint(51, (key), (deflt))
#define SetIsChainStart(key, _add, _replace)   \
		SetDefTableKey(52, (key), (_add), (_replace))
#define ResetIsChainStart(key, _val)   \
		ResetDefTableKey(52, (key), (_val))
#define GetIsChainStart(key, deflt)   \
		GetDefTableKey(52, (key), (deflt))
#define SetIsPlain(key, _add, _replace)   \
		Setint(53, (key), (_add), (_replace))
#define ResetIsPlain(key, _val)   \
		Resetint(53, (key), (_val))
#define GetIsPlain(key, deflt)   \
		Getint(53, (key), (deflt))
#define SetEmptyHEADChain(key, _add, _replace)   \
		SetDefTableKey(54, (key), (_add), (_replace))
#define ResetEmptyHEADChain(key, _val)   \
		ResetDefTableKey(54, (key), (_val))
#define GetEmptyHEADChain(key, deflt)   \
		GetDefTableKey(54, (key), (deflt))
#define SetEmptyHEADAttr(key, _add, _replace)   \
		SetDefTableKey(55, (key), (_add), (_replace))
#define ResetEmptyHEADAttr(key, _val)   \
		ResetDefTableKey(55, (key), (_val))
#define GetEmptyHEADAttr(key, deflt)   \
		GetDefTableKey(55, (key), (deflt))
#define SetEmptyHEADCompsOfRule(key, _add, _replace)   \
		SetDefTableKeyList(56, (key), (_add), (_replace))
#define ResetEmptyHEADCompsOfRule(key, _val)   \
		ResetDefTableKeyList(56, (key), (_val))
#define GetEmptyHEADCompsOfRule(key, deflt)   \
		GetDefTableKeyList(56, (key), (deflt))
#define SetRemoteAttr(key, _add, _replace)   \
		Setint(57, (key), (_add), (_replace))
#define ResetRemoteAttr(key, _val)   \
		Resetint(57, (key), (_val))
#define GetRemoteAttr(key, deflt)   \
		Getint(57, (key), (deflt))
#define SetIsIncluding(key, _add, _replace)   \
		Setint(58, (key), (_add), (_replace))
#define ResetIsIncluding(key, _val)   \
		Resetint(58, (key), (_val))
#define GetIsIncluding(key, deflt)   \
		Getint(58, (key), (deflt))
#define SetIsConstituent(key, _add, _replace)   \
		Setint(59, (key), (_add), (_replace))
#define ResetIsConstituent(key, _val)   \
		Resetint(59, (key), (_val))
#define GetIsConstituent(key, deflt)   \
		Getint(59, (key), (deflt))
#define SetIsConstituents(key, _add, _replace)   \
		Setint(60, (key), (_add), (_replace))
#define ResetIsConstituents(key, _val)   \
		Resetint(60, (key), (_val))
#define GetIsConstituents(key, deflt)   \
		Getint(60, (key), (deflt))
#define SetRemoteSet(key, _add, _replace)   \
		SetSymbAttrList(61, (key), (_add), (_replace))
#define ResetRemoteSet(key, _val)   \
		ResetSymbAttrList(61, (key), (_val))
#define GetRemoteSet(key, deflt)   \
		GetSymbAttrList(61, (key), (deflt))
#define SetShieldSet(key, _add, _replace)   \
		SetDefTableKeyList(62, (key), (_add), (_replace))
#define ResetShieldSet(key, _val)   \
		ResetDefTableKeyList(62, (key), (_val))
#define GetShieldSet(key, deflt)   \
		GetDefTableKeyList(62, (key), (deflt))
#define SetFunct2(key, _add, _replace)   \
		Setint(63, (key), (_add), (_replace))
#define ResetFunct2(key, _val)   \
		Resetint(63, (key), (_val))
#define GetFunct2(key, deflt)   \
		Getint(63, (key), (deflt))
#define SetFunct1(key, _add, _replace)   \
		Setint(64, (key), (_add), (_replace))
#define ResetFunct1(key, _val)   \
		Resetint(64, (key), (_val))
#define GetFunct1(key, deflt)   \
		Getint(64, (key), (deflt))
#define SetFunct0(key, _add, _replace)   \
		Setint(65, (key), (_add), (_replace))
#define ResetFunct0(key, _val)   \
		Resetint(65, (key), (_val))
#define GetFunct0(key, deflt)   \
		Getint(65, (key), (deflt))
#define SetInComputations(key, _add, _replace)   \
		SetDefTableKeyList(66, (key), (_add), (_replace))
#define ResetInComputations(key, _val)   \
		ResetDefTableKeyList(66, (key), (_val))
#define GetInComputations(key, deflt)   \
		GetDefTableKeyList(66, (key), (deflt))
#define SetInVoidContext(key, _add, _replace)   \
		Setint(67, (key), (_add), (_replace))
#define ResetInVoidContext(key, _val)   \
		Resetint(67, (key), (_val))
#define GetInVoidContext(key, deflt)   \
		Getint(67, (key), (deflt))
#define SetInValueContext(key, _add, _replace)   \
		Setint(68, (key), (_add), (_replace))
#define ResetInValueContext(key, _val)   \
		Resetint(68, (key), (_val))
#define GetInValueContext(key, deflt)   \
		Getint(68, (key), (deflt))
#define SetRemoteEpxrIsErr(key, _add, _replace)   \
		Setint(69, (key), (_add), (_replace))
#define ResetRemoteEpxrIsErr(key, _val)   \
		Resetint(69, (key), (_val))
#define GetRemoteEpxrIsErr(key, deflt)   \
		Getint(69, (key), (deflt))
#define SetCompRepr(key, _add, _replace)   \
		SetPExpr(70, (key), (_add), (_replace))
#define ResetCompRepr(key, _val)   \
		ResetPExpr(70, (key), (_val))
#define GetCompRepr(key, deflt)   \
		GetPExpr(70, (key), (deflt))
#define SetAllRuleComps(key, _add, _replace)   \
		SetDefTableKeyList(71, (key), (_add), (_replace))
#define ResetAllRuleComps(key, _val)   \
		ResetDefTableKeyList(71, (key), (_val))
#define GetAllRuleComps(key, deflt)   \
		GetDefTableKeyList(71, (key), (deflt))
#define SetIsLoopInit(key, _add, _replace)   \
		Setint(72, (key), (_add), (_replace))
#define ResetIsLoopInit(key, _val)   \
		Resetint(72, (key), (_val))
#define GetIsLoopInit(key, deflt)   \
		Getint(72, (key), (deflt))
#define SetIsGenTreeDummy(key, _add, _replace)   \
		Setint(73, (key), (_add), (_replace))
#define ResetIsGenTreeDummy(key, _val)   \
		Resetint(73, (key), (_val))
#define GetIsGenTreeDummy(key, deflt)   \
		Getint(73, (key), (deflt))
#define SetInRemoteSet(key, _add, _replace)   \
		Setint(74, (key), (_add), (_replace))
#define ResetInRemoteSet(key, _val)   \
		Resetint(74, (key), (_val))
#define GetInRemoteSet(key, deflt)   \
		Getint(74, (key), (deflt))
#define SetDerivableVisited(key, _add, _replace)   \
		Setint(75, (key), (_add), (_replace))
#define ResetDerivableVisited(key, _val)   \
		Resetint(75, (key), (_val))
#define GetDerivableVisited(key, deflt)   \
		Getint(75, (key), (deflt))
#define SetDid(key, _add, _replace)   \
		Setint(76, (key), (_add), (_replace))
#define ResetDid(key, _val)   \
		Resetint(76, (key), (_val))
#define GetDid(key, deflt)   \
		Getint(76, (key), (deflt))
#define SetRuleOfAttr(key, _add, _replace)   \
		SetDefTableKey(77, (key), (_add), (_replace))
#define ResetRuleOfAttr(key, _val)   \
		ResetDefTableKey(77, (key), (_val))
#define GetRuleOfAttr(key, deflt)   \
		GetDefTableKey(77, (key), (deflt))
#define SetHasAccuAsgn(key, _add, _replace)   \
		Setint(78, (key), (_add), (_replace))
#define ResetHasAccuAsgn(key, _val)   \
		Resetint(78, (key), (_val))
#define GetHasAccuAsgn(key, deflt)   \
		Getint(78, (key), (deflt))
#define SetHasNonAccuAsgn(key, _add, _replace)   \
		Setint(79, (key), (_add), (_replace))
#define ResetHasNonAccuAsgn(key, _val)   \
		Resetint(79, (key), (_val))
#define GetHasNonAccuAsgn(key, deflt)   \
		Getint(79, (key), (deflt))
#define SetInhAccu(key, _add, _replace)   \
		Setint(80, (key), (_add), (_replace))
#define ResetInhAccu(key, _val)   \
		Resetint(80, (key), (_val))
#define GetInhAccu(key, deflt)   \
		Getint(80, (key), (deflt))
#define SetInhAccuVisits(key, _add, _replace)   \
		Setint(81, (key), (_add), (_replace))
#define ResetInhAccuVisits(key, _val)   \
		Resetint(81, (key), (_val))
#define GetInhAccuVisits(key, deflt)   \
		Getint(81, (key), (deflt))
#define SetIsAccu(key, _add, _replace)   \
		Setint(82, (key), (_add), (_replace))
#define ResetIsAccu(key, _val)   \
		Resetint(82, (key), (_val))
#define GetIsAccu(key, deflt)   \
		Getint(82, (key), (deflt))
#define SetAccuLhs(key, _add, _replace)   \
		SetPExpr(83, (key), (_add), (_replace))
#define ResetAccuLhs(key, _val)   \
		ResetPExpr(83, (key), (_val))
#define GetAccuLhs(key, deflt)   \
		GetPExpr(83, (key), (deflt))
#define SetAccuExecList(key, _add, _replace)   \
		SetPExprList(84, (key), (_add), (_replace))
#define ResetAccuExecList(key, _val)   \
		ResetPExprList(84, (key), (_val))
#define GetAccuExecList(key, deflt)   \
		GetPExprList(84, (key), (deflt))
#define SetAccuDepList(key, _add, _replace)   \
		SetPExprList(85, (key), (_add), (_replace))
#define ResetAccuDepList(key, _val)   \
		ResetPExprList(85, (key), (_val))
#define GetAccuDepList(key, deflt)   \
		GetPExprList(85, (key), (deflt))

extern struct PropList PDLk[];

#define NODEPTRkey (&PDLk[0])
#define POSITIONkey (&PDLk[1])
#define SYMBkey (&PDLk[2])
#define BOOLkey (&PDLk[3])
#define intkey (&PDLk[4])
#define VOID_LISTkey (&PDLk[5])
#define VOIDkey (&PDLk[6])
#define ROOTCLASSkey (&PDLk[7])
#define NULLNODEPTRkey (&PDLk[8])
#define IFkey (&PDLk[9])
#define ORDERkey (&PDLk[10])
#define RuleFctkey (&PDLk[11])
#define RhsFctkey (&PDLk[12])
#define TermFctkey (&PDLk[13])
#define LINEkey (&PDLk[14])
#define COLkey (&PDLk[15])
#define COORDREFkey (&PDLk[16])
#define RULENAMEkey (&PDLk[17])
#define ErrorType (&PDLk[18])
#define HEADKey (&PDLk[19])
#define TAILKey (&PDLk[20])
#define GENTREEkey (&PDLk[21])

extern DefTableKey CloneKey ELI_ARG((DefTableKey key));
/* Clone a definition
 *    On entry-
 *       key=a valid definition
 *    On exit-
 *       CloneKey=Unique definition with the same properties
 *                and property values as key
 ***/

#ifdef MONITOR
extern void pdl_mon_properties ELI_ARG((DefTableKey));
#endif

#endif
