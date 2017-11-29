/**
 * @file  SciRegexSearch.cxx
 * @brief integrate DeelX and TRE regex searching for Scintilla library
 *              (Scintilla Lib is copyright 1998-2016 by Neil Hodgson <neilh@scintilla.org>)
 *
 *        uses DEELX - Regular Expression Engine (v1.3) (deelx.h) - http://www.regexlab.com/deelx/
 *               download: http://www.regexlab.com/download/deelx/deelx.zip  (v1.2)
 *               or      : https://github.com/AndreasMartin72/mksqlite/blob/master/deelx/deelx.h  (v1.3)
 *               (Copyright Announcement: Free to use/redistribute. Provenance must be declared when redistributed)
 *               API documentation see accompanying "deelx_en.chm" HTML Help.
 *
 * @autor Rainer Kottenhoff (RaPeHoff)
 *
 * Install:
 *   - place files (deelx64.h, SciRegexSearch.cxx, deelx_en.chm)
 *       in a directory (deelx) within the scintilla project (.../scintilla/deelx/)
 *   - add source files to scintilla project (Scintilla.vcxproj in VS)
 *   - define compiler (preprocessor) macro for scintilla project named "SCI_OWNREGEX"
 *       -> this will switch from scintilla's buildin regex engine to deelx's regex engine
 *   - recompile and link scintilla library
 *   - build application
 */

#ifdef SCI_OWNREGEX

#include <stdlib.h>
#include <string>
#include <vector>

#define VC_EXTRALEAN 1
#include <windows.h>

#pragma warning( push )
#pragma warning( disable : 4996 )   // Scintilla's "unsafe" use of std::copy() (SplitVector.h)
 //                                  // or use -D_SCL_SECURE_NO_WARNINGS preprocessor define

#include "Platform.h"
#include "Scintilla.h"
#include "ILexer.h"
#include "SplitVector.h"
#include "Partitioning.h"
#include "CellBuffer.h"
#include "CaseFolder.h"
#include "RunStyles.h"
#include "Decoration.h"
#include "CharClassify.h"
#include "Document.h"
// ---------------------------------------------------------------
#include "deelx64.h"   // DEELX - Regular Expression Engine (v1.3)
#include "tre.h"       // TRE - Regular Expression Engine (v0.8)
// ---------------------------------------------------------------

using namespace Scintilla;

#define SCFIND_NP3_FUZZY_BIT SCFIND_APPROXIMATE
const int APPROXIMAT_MAX_VAL = 100;  // max: 0xFF

#define SciPos(pos)    static_cast<Sci::Position>(pos)
#define SciLn(line)    static_cast<Sci::Line>(line)
#define SciPosExt(pos) static_cast<Sci_Position>(pos)

#define DeelXPos(pos)  static_cast<deelx::index_t>(pos)
#define TrePos(pos)    static_cast<int>(pos)

#define Cast2long(n)   static_cast<long>(n)

enum SearchEngine { DEELX_ENGINE, TRE_ENGINE };

// ---------------------------------------------------------------

const int MAX_GROUP_COUNT = 10;

// ---------------------------------------------------------------

// sone forward declarations 
static std::string& translateRegExpr(std::string& regExprStr, bool wholeWord, bool wordStart, int eolMode);
static std::string& convertReplExpr(std::string& replStr);

// ---------------------------------------------------------------

class DeelXRegExEngine
{
public:

  explicit DeelXRegExEngine(CharClassify* charClassTable)
    : m_RegExprStrg()
    , m_CompileFlags(-1)
    , m_RegExpr()
    , m_Match()
    , m_MatchPos(-1)
    , m_MatchLength(0)
    , m_SubstBuffer()
  {}

  virtual ~DeelXRegExEngine()
  {
    ReleaseSubstitutionBuffer();
    m_RegExprStrg.clear();
  }

  long FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char* pattern,
                        bool caseSensitive, bool word, bool wordStart, int flags, Sci::Position* length);

  const char* SubstituteByPosition(Document* doc, const char* text, Sci::Position* length);

private:

  __inline void ReleaseSubstitutionBuffer()
  {
    //if (m_SubstitutionBuffer) {
    //  m_RegExpr.ReleaseString(m_SubstitutionBuffer);
    //  m_SubstitutionBuffer = nullptr;
    //}
  }

private:

  std::string m_RegExprStrg;
  int m_CompileFlags;
  deelx::CRegexpT<char> m_RegExpr;
  deelx::MatchResult m_Match;
  Sci::Position m_MatchPos;
  Sci::Position m_MatchLength;
  std::string m_SubstBuffer;

};
// ============================================================================



class TREgExEngine
{
public:

  explicit TREgExEngine(CharClassify* charClassTable)
    : m_FindRegExPattern()
    , m_ApproximateVal(0)
    , m_CompileFlags(-1)
    , m_CompiledRegExPattern()
    , m_CompileResult(REG_NOMATCH)
    , m_Groups()
    , m_RangeDocBegin(0)
    , m_RangeLength(0)
    , m_SubstBuffer()
  {
    m_CompiledRegExPattern = { 0, nullptr };
  }

  virtual ~TREgExEngine()
  {
    if (m_CompiledRegExPattern.value)
      tre_regfree(&m_CompiledRegExPattern);
  }

  long FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char* pattern,
    bool caseSensitive, bool word, bool wordStart, int flags, Sci::Position* length);

  const char* SubstituteByPosition(Document* doc, const char* text, Sci::Position* length);


private:

  int getDocContextMatchFlags(Document* doc, Sci::Position rangeBegin, Sci::Position rangeEnd);

  void adjustToEOLMode(Document* doc, int& matchResult);


private:

  std::string m_FindRegExPattern;
  int m_ApproximateVal;
  int m_CompileFlags;
  regex_t m_CompiledRegExPattern;
  int m_CompileResult;

  static const size_t MAXGROUPS = 10;
  regmatch_t m_Groups[MAXGROUPS];

  Sci::Position  m_RangeDocBegin;
  Sci::Position  m_RangeLength;

  std::string m_SubstBuffer;
};
// ============================================================================




/**
 * Find text in document, supporting both forward and backward
 * searches (just pass minPos > maxPos to do a backward search)
 * Has not been tested with backwards DBCS searches yet.
 */
long DeelXRegExEngine::FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char *pattern,
                                bool caseSensitive, bool word, bool wordStart, int searchFlags, Sci::Position *length)
{
  const bool right2left = false; // always left-to-right match mode
  const bool extended = false;   // ignore spaces and use '#' as line-comment)

  // Range endpoints should not be inside DBCS characters, but just in case, move them.
  minPos = doc->MovePositionOutsideChar(minPos,1,false);
  maxPos = doc->MovePositionOutsideChar(maxPos,1,false);
  const bool findprevious = (minPos > maxPos);

  int compileFlags = deelx::NO_FLAG;
  compileFlags |= (deelx::MULTILINE | deelx::GLOBAL); // the .(dot) does not match line-breaks
  //compileFlags |= (deelx::SINGLELINE | deelx::MULTILINE | deelx::GLOBAL);  // the .(dot) also matches line-breaks

  compileFlags |= (extended) ? deelx::EXTENDED : deelx::NO_FLAG;
  compileFlags |= (caseSensitive) ? deelx::NO_FLAG : deelx::IGNORECASE;
  compileFlags |= (right2left) ? deelx::RIGHTTOLEFT : deelx::NO_FLAG;

  std::string sRegExprStrg = translateRegExpr(std::string(pattern),word,wordStart,-1);

  bool bReCompile = (m_CompileFlags != compileFlags) || (m_RegExprStrg.compare(sRegExprStrg) != 0);

  if (bReCompile) {
    m_RegExprStrg = sRegExprStrg;
    m_CompileFlags = compileFlags;
    try {
      m_RegExpr.Compile(m_RegExprStrg.c_str(), m_CompileFlags);
    }
    catch (...) {
      return -2;  // -1 is normally used for not found, -2 is used here for invalid regex
      // DeelX is very fault tolerant and assumes what the user may want ... :-/
      // so -2 may not occur!
    }
  }

  Sci::Position rangeBegin = (findprevious) ? maxPos : minPos;
  Sci::Position rangeEnd   = (findprevious) ? minPos : maxPos;
  
  Sci::Line     linesTotal  = doc->LinesTotal();
  Sci::Position fileLastPos = SciPos(doc->Length());

  Sci::Line lineOfBegPos = SciLn(doc->LineFromPosition(SciPosExt(rangeBegin)));
  Sci::Line lineOfEndPos = SciLn(doc->LineFromPosition(SciPosExt(rangeEnd)));

  Sci::Position lineStartOfBegPos = SciPos(doc->LineStart(SciPosExt(lineOfBegPos)));
  Sci::Position lineEndOfEndPos   = SciPos(doc->LineEnd(SciPosExt(lineOfEndPos)));

  // --- adapt range start/end according to search pattern ---

  size_t begMetaPos = m_RegExprStrg.find_first_of('^');
  bool bFoundBegMeta = (begMetaPos != std::string::npos) && 
                       ((begMetaPos == 0) || (m_RegExprStrg.find_first_of('\\') != (begMetaPos - 1)));
  if (bFoundBegMeta) {
    if (lineStartOfBegPos != rangeBegin) {
      rangeBegin = SciPos((lineOfBegPos < linesTotal) ? 
                          doc->LineStart(SciPosExt(lineOfBegPos + 1)) : 
                          doc->LineEnd(SciPosExt(linesTotal)));
      rangeEnd   = (rangeBegin <= rangeEnd) ? rangeEnd : rangeBegin;
    }
  }

  size_t endMetaPos = m_RegExprStrg.find_last_of('$');
  bool bFoundEndMeta = (endMetaPos != std::string::npos) && 
                       ((endMetaPos == 0) || (m_RegExprStrg.find_last_of('\\') != (endMetaPos - 1)));
  if (bFoundEndMeta) {
    if (lineEndOfEndPos != rangeEnd) {
      rangeEnd   = SciPos((0 < lineOfEndPos) ? doc->LineEnd(SciPosExt(lineOfEndPos - 1)) : 0);
      rangeBegin = (rangeBegin <= rangeEnd) ? rangeBegin : rangeEnd;
    }
  }

  // ---  start search  ---

  m_MatchPos    = SciPos(-1); // not found
  m_MatchLength = SciPos(0);
  const deelx::index_t searchStop = DeelXPos(rangeEnd);

  if (findprevious)  // search previous 
  {
    deelx::CContext* pContext = m_RegExpr.PrepareMatch(doc->RangePointer(0, fileLastPos), 0);
    m_Match = m_RegExpr.Match(pContext);
    // search for last occurrence in range
    while (m_Match.IsMatched() && (m_Match.GetStart() < searchStop))
    {
      m_MatchPos = SciPos(m_Match.GetStart());
      m_MatchLength = SciPos(m_Match.GetEnd() - m_Match.GetStart());
      m_Match = m_RegExpr.Match(pContext); //next
    }
    m_RegExpr.ReleaseContext(pContext);
  }
  else {
    m_Match = m_RegExpr.Match(doc->RangePointer(0, fileLastPos), 
                              DeelXPos(fileLastPos), DeelXPos(rangeBegin));
    if (m_Match.IsMatched() && (m_Match.GetStart() < searchStop)) {
      m_MatchPos = SciPos(m_Match.GetStart());
      m_MatchLength = SciPos(m_Match.GetEnd() - m_Match.GetStart());
    }
  }

  //NOTE: potential 64-bit-size issue at interface here:
  *length = SciPos(m_MatchLength);
  return static_cast<long>(m_MatchPos);
}
// ============================================================================


#if 0

#define _MAX(a,b) ((a)>(b)?(a):(b))
#define _MIN(a,b) ((a)<(b)?(a):(b))


const char* DeelXRegExEngine::SubstituteByPosition(Document* doc, const char* text, Sci::Position* length)
{
  if (!m_Match.IsMatched() || (m_MatchPos < 0)) {
    *length = SciPos(0);
    return nullptr;
  }

  Sci::Position fileLastPos = SciPos(doc->Length());

  std::string rawReplStrg = convertReplExpr(std::string(text,*length));
  deelx::index_t rawReplLength = DeelXPos(rawReplStrg.length());

  // document slice with look-around context (+/- MatchLength ???)
  Sci::Position looka = m_MatchLength;   // what should be the size of the lookahead ???

  Sci::Position begin = _MAX(0, (m_MatchPos - looka));
  Sci::Position end   = _MIN((m_MatchPos + (2 * looka)), fileLastPos);
  Sci::Position len   = (end - begin);
  const char* pDocumentSlice = doc->RangePointer(begin, len);

  deelx::index_t relMatchPos = DeelXPos(m_MatchPos - begin);
  deelx::index_t relSliceLen = DeelXPos(len);

  ReleaseSubstitutionBuffer();
  deelx::index_t resLength_out = rawReplLength; // init
  m_SubstitutionBuffer = m_RegExpr.Replace(pDocumentSlice, relSliceLen, rawReplStrg.c_str(), rawReplLength, resLength_out, relMatchPos, 1, &m_Match, nullptr);

  //NOTE: potential 64-bit-size issue at interface here:
  deelx::index_t realReplLength = SciPos(resLength_out - relSliceLen + m_MatchLength);
  *length = SciPos(realReplLength);

  // cut out replacement sub-string only !!!
  m_SubstitutionBuffer[relMatchPos + realReplLength + 1] = '\0';
  return &(m_SubstitutionBuffer[relMatchPos]);
}
// ============================================================================
#endif


const char* DeelXRegExEngine::SubstituteByPosition(Document* doc, const char* text, Sci::Position* length)
{
  std::string rawReplStrg = convertReplExpr(std::string(text, *length));

  if (m_Match.IsMatched() != 0) {

    m_SubstBuffer.clear();

    for (size_t j = 0; j < rawReplStrg.length(); j++) {
      if ((rawReplStrg[j] == '$') || (rawReplStrg[j] == '\\'))
      {
        if ((rawReplStrg[j + 1] >= '0') && (rawReplStrg[j + 1] <= '9'))
        {
          deelx::index_t grpNum = rawReplStrg[j + 1] - '0';

          if (grpNum <= m_Match.MaxGroupNumber())
          {
            deelx::index_t gStart = m_Match.GetGroupStart(grpNum);
            deelx::index_t len = m_Match.GetGroupEnd(grpNum) - gStart;

            m_SubstBuffer.append(doc->RangePointer(SciPos(gStart), SciPos(len)), len);
          }
          ++j;
        }
        else if (rawReplStrg[j] == '\\') {
          m_SubstBuffer.push_back('\\');
          ++j;
        }
        else {
          m_SubstBuffer.push_back(rawReplStrg[j]);
        }
      }
      else {
        m_SubstBuffer.push_back(rawReplStrg[j]);
      }
    }
  }
  else {
    m_SubstBuffer = rawReplStrg;
  }

  //NOTE: potential 64-bit-size issue at interface here:
  *length = SciPos(m_SubstBuffer.length());
  return m_SubstBuffer.c_str();
}
// ============================================================================




// ============================================================================
//  TREgExEngine  methods
// ============================================================================


/**
* Find text in document, supporting both forward and backward
* searches (just pass minPos > maxPos to do a backward search)
* Has not been tested with backwards DBCS searches yet.
*/
long TREgExEngine::FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char *pattern,
  bool caseSensitive, bool word, bool wordStart, int flags, Sci::Position *length)
{
  const bool right2left = false; // always left-to-right match mode
  const bool extended = true;    // ignore spaces and use '#' as line-comment)

  // retrieve approximate value from flags
  int iApproxVal = (flags & (0xFF << 8)) >> 8;

  // Range endpoints should not be inside DBCS characters, but just in case, move them.
  minPos = doc->MovePositionOutsideChar(minPos, 1, false);
  maxPos = doc->MovePositionOutsideChar(maxPos, 1, false);
  const bool bFindprevious = (minPos > maxPos);

  if (bFindprevious) {
    Sci::Position tmp = minPos;  minPos = maxPos;  maxPos = tmp;
  }

  int compileFlags = REG_BASIC;
  compileFlags |= REG_NEWLINE; // the .(dot) does not match line-breaks
  compileFlags |= (extended) ? REG_EXTENDED : REG_BASIC;
  compileFlags |= (caseSensitive) ? REG_BASIC : REG_ICASE;
  compileFlags |= (right2left) ? REG_RIGHT_ASSOC : REG_BASIC;

  std::string transPattern = translateRegExpr(std::string(pattern), word, wordStart, doc->eolMode);

  bool bReCompile = ((m_CompileFlags != compileFlags) || (m_FindRegExPattern.compare(transPattern) != 0));

  if (bReCompile)
  {
    if (m_CompiledRegExPattern.value) {
      tre_regfree(&m_CompiledRegExPattern);
      m_CompiledRegExPattern.value = nullptr;
      m_CompiledRegExPattern.re_nsub = 0;
    }
    m_CompileFlags = compileFlags;
    m_FindRegExPattern = transPattern;
    try {
      m_CompileResult = tre_regcomp(&m_CompiledRegExPattern, m_FindRegExPattern.c_str(), compileFlags);
    }
    catch (...) {
      return (0 - REG_BADPAT);  // -1 is normally used for not found, -2 is used here for invalid regex
    }
  }

  //if (m_CompiledRegExPattern.re_nsub >= MAXGROUPS) {
  //  //TODO: show warning
  //}

  switch (m_CompileResult) {
  case REG_OK:
    // all right
    break;
  case REG_NOMATCH:
    // n.a.
  case REG_BADPAT:
    // Invalid regexp.TRE returns this only if a multibyte character set is used in the current locale, and regex contained an invalid multibyte sequence.
  case REG_ECOLLATE:
    // Invalid collating element referenced.TRE returns this whenever equivalence classes or multicharacter collating elements are used in bracket expressions(they are not supported yet).
  case REG_ECTYPE:
    // Unknown character class name in[[:name:]].
  case REG_EESCAPE:
    // The last character of regex was a backslash(\).
  case REG_ESUBREG:
    // Invalid back reference; number in \digit invalid.
  case REG_EBRACK:
    // [] imbalance.
  case REG_EPAREN:
    // \(\) or () imbalance.
  case REG_EBRACE:
    // \{\} or {} imbalance.
  case REG_BADBR:
    // {} content invalid : not a number, more than two numbers, first larger than second, or number too large.
  case REG_ERANGE:
    // Invalid character range, e.g.ending point is earlier in the collating order than the starting point.
  case REG_ESPACE:
    // Out of memory, or an internal limit exceeded.
  case REG_BADRPT:
    // Invalid use of repetition operators : two or more repetition operators have been chained in an undefined way.
  default:
    return (0 - m_CompileResult);
  }

  // --- adjust line start and end positions for match ---
  
  m_RangeDocBegin = minPos;
  m_RangeLength = (maxPos - minPos);
  m_ApproximateVal = iApproxVal;

  const int cost_ins   = 1;  // The default cost of an inserted character, that is, an extra character in string.
  const int cost_del   = 1;  // The default cost of a deleted character, that is, a character missing from string.
  const int cost_subst = 1;  // The default cost of a substituted character.

  const int base = (int)m_FindRegExPattern.length();

  // The maximum allowed cost of a match. 
  // If this is set to zero, an exact matching is searched for, 
  // and results equivalent to those returned by the regexec() functions are returned.
  const int max_cost  = (int)((base * m_ApproximateVal) / APPROXIMAT_MAX_VAL);

  const int max_ins   = max_cost;   // Maximum allowed number of inserted characters.
  const int max_del   = max_cost;   // Maximum allowed number of deleted characters.
  const int max_subst = max_cost;   // Maximum allowed number of substituted characters.
  const int max_err   = max_cost;   // Maximum allowed number of errors (inserts + deletes + substitutes)
  
  const regaparams_t approxParams = { cost_ins, cost_del, cost_subst, max_cost, max_ins, max_del, max_subst, max_err};


  if (bFindprevious)
  {
    // search for last occurrence in range
    Sci::Position rangeBegin = m_RangeDocBegin;
    Sci::Position rangLength = (maxPos - m_RangeDocBegin);
    
    regamatch_t approxMatch = { 1, &(m_Groups[0]), 0, 0, 0, 0 };

    int prvmatch = REG_NOMATCH;
    do {
      prvmatch = tre_reganexec(&m_CompiledRegExPattern, // don't care for sub groups in while loop
      doc->RangePointer(rangeBegin, rangLength), (size_t)rangLength, &approxMatch, approxParams,
      getDocContextMatchFlags(doc, rangeBegin, rangeBegin + rangLength));

      if (prvmatch == REG_OK) {
        // save Range
        m_RangeDocBegin = rangeBegin;
        m_RangeLength = (maxPos - rangeBegin);
        // prepare next match
        rangeBegin += ((m_Groups[0].rm_eo - m_Groups[0].rm_so) > 0) ? (m_Groups[0].rm_eo) : 1;
        rangLength = (maxPos - rangeBegin);
      }
    } while ((prvmatch == REG_OK) && (rangeBegin < maxPos));
  }
  else { // find forward

    // we don't want to find the match with 'lowest cost in range', 
    // but the first match, which 'fits cost limit' !!

    Sci::Position rangLength = (maxPos - m_RangeDocBegin);

    regamatch_t approxMatch = { 1, &(m_Groups[0]), 0, 0, 0, 0 };

    int xmatch = REG_NOMATCH;
    do {
      xmatch = tre_reganexec(&m_CompiledRegExPattern, // don't care for sub groups in while loop
        doc->RangePointer(m_RangeDocBegin, rangLength), (size_t)rangLength, &approxMatch, approxParams,
        getDocContextMatchFlags(doc, m_RangeDocBegin, m_RangeDocBegin + rangLength));

      if (xmatch == REG_OK) {
        m_RangeLength = rangLength; // remember good find
        // shorten range length
        rangLength = m_Groups[0].rm_so;
      }
    } while ((xmatch == REG_OK) && (m_Groups[0].rm_so > 0));
  }

  // --- find match in new range and set region groups ---
  size_t nsubgrp = min(1 + m_CompiledRegExPattern.re_nsub, MAXGROUPS);
  regamatch_t approxMatch = { nsubgrp, &(m_Groups[0]), 0, 0, 0, 0 };

  int match = tre_reganexec(&m_CompiledRegExPattern,
    doc->RangePointer(m_RangeDocBegin, m_RangeLength), (size_t)m_RangeLength, &approxMatch, approxParams,
    getDocContextMatchFlags(doc, m_RangeDocBegin, m_RangeDocBegin + m_RangeLength));

  adjustToEOLMode(doc, match);

  //NOTE: potential 64-bit-size issue at interface here:
  *length = ((match == REG_OK) ? SciPos(m_Groups[0].rm_eo - m_Groups[0].rm_so) : -1);
  return  ((match == REG_OK) ? Cast2long(m_RangeDocBegin + SciPos(m_Groups[0].rm_so)) : Cast2long(0 - REG_NOMATCH));
}
// ============================================================================




const char* TREgExEngine::SubstituteByPosition(Document* doc, const char* text, Sci::Position* length)
{
  std::string rawReplStrg = convertReplExpr(std::string(text, *length));

  if (m_Groups[0].rm_so >= 0) {

    m_SubstBuffer.clear();

    for (size_t j = 0; j < rawReplStrg.length(); j++) {
      if ((rawReplStrg[j] == '$') || (rawReplStrg[j] == '\\'))
      {
        if ((rawReplStrg[j + 1] >= '0') && (rawReplStrg[j + 1] <= '9'))
        {
          unsigned int grpNum = rawReplStrg[j + 1] - '0';

          Sci::Position len = (m_Groups[grpNum].rm_eo - m_Groups[grpNum].rm_so);

          if (len > 0) // Will be 0 if try for a match that did not occur
            m_SubstBuffer.append(doc->RangePointer(SciPos(m_RangeDocBegin + m_Groups[grpNum].rm_so), SciPos(len)), len);

          ++j;
        }
        else if (rawReplStrg[j] == '\\') {
          m_SubstBuffer.push_back('\\');
          ++j;
        }
        else {
          m_SubstBuffer.push_back(rawReplStrg[j]);
        }
      }
      else {
        m_SubstBuffer.push_back(rawReplStrg[j]);
      }
    }
  }
  else {
    m_SubstBuffer = rawReplStrg;
  }

  //NOTE: potential 64-bit-size issue at interface here:
  *length = SciPos(m_SubstBuffer.length());
  return m_SubstBuffer.c_str();

}
// ============================================================================



// ----------------------------------------------------------------------------------------------
// --- correct EOL ($) matches:  TRE's  $  matches only empty string immediately before '\n'  ---
// ----------------------------------------------------------------------------------------------


void TREgExEngine::adjustToEOLMode(Document* doc, int& matchResult)
{
  if (matchResult == REG_OK) {

    switch (doc->eolMode) {
    case SC_EOL_LF:
      // we are fine here
      break;

    case SC_EOL_CR:
      //TODO: don't know what to do here ...
      break;

    case SC_EOL_CRLF:
    {
      char eos = (!m_FindRegExPattern.empty()) ? m_FindRegExPattern.back() : '\0';
      char esc = (m_FindRegExPattern.length() >= 2) ? m_FindRegExPattern.at(m_FindRegExPattern.length() - 2) : '\0';

      if (('$' == eos) && ('\\' != esc)) {
        Sci::Position matchRelEnd = m_Groups[0].rm_eo;
        Sci::Position matchEndDocPos = m_RangeDocBegin + matchRelEnd - 1;
        if ((matchEndDocPos >= 0) && ('\r' == doc->CharAt(matchEndDocPos))) {
          for (auto& grp : m_Groups) {
            if (grp.rm_so == matchRelEnd) { --(grp.rm_so); }
            if (grp.rm_eo == matchRelEnd) { --(grp.rm_eo); }
          }
        }
        else { // not a CRLF EOL - wrong match
          matchResult = REG_NOMATCH;
          m_Groups[0].rm_so = -1;
          m_Groups[0].rm_eo = -1;
        }
      }
    }
    break;
    }
  }
}
// ============================================================================



int TREgExEngine::getDocContextMatchFlags(Document* doc, Sci::Position rangeBegin, Sci::Position rangeEnd)
{
  //Sci::Line linesTotal = doc->LinesTotal();
  Sci::Position fileLastPos = SciPos(doc->Length());

  Sci::Position lineOfMinPos = SciPos(doc->LineFromPosition(SciPosExt(rangeBegin)));
  Sci::Position lineOfMaxPos = SciPos(doc->LineFromPosition(SciPosExt(rangeEnd)));

  Sci::Position lineStartOfMinPos = SciPos(doc->LineStart(lineOfMinPos));
  Sci::Position lineEndOfMaxPos = SciPos(doc->LineEnd(lineOfMaxPos));

  int matchFlags = REG_NEWLINE;
  matchFlags |= (lineStartOfMinPos == rangeBegin) ? 0 : REG_NOTBOL;
  matchFlags |= (lineEndOfMaxPos == rangeEnd) ? 0 : REG_NOTEOL;

  return matchFlags;
}
// ============================================================================




// ============================================================================
// extending Scintilla's  RegexSearchBase
// ============================================================================


class SciRegexSearch : public RegexSearchBase
{
public:

  explicit SciRegexSearch(CharClassify* charClassTable)
    : m_DeelXEngine(charClassTable)
    , m_TreEngine(charClassTable)
    , m_LastSearchEngine(DEELX_ENGINE)
  {}

  virtual ~SciRegexSearch()
  {
  }

  virtual long FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char* pattern,
                        bool caseSensitive, bool word, bool wordStart, int flags, Sci::Position* length) override;

  virtual const char* SubstituteByPosition(Document* doc, const char* text, Sci::Position* length) override;


private:

  DeelXRegExEngine m_DeelXEngine;
  TREgExEngine     m_TreEngine;
  
  SearchEngine     m_LastSearchEngine; 
};
// ============================================================================


RegexSearchBase *Scintilla::CreateRegexSearch(CharClassify *charClassTable)
{
  return new SciRegexSearch(charClassTable);
}

// ============================================================================


long SciRegexSearch::FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char* pattern,
                              bool caseSensitive, bool word, bool wordStart, int searchFlags, Sci::Position* length)
{
  if (searchFlags & SCFIND_NP3_FUZZY_BIT) 
  {
    m_LastSearchEngine = TRE_ENGINE;
    return m_TreEngine.FindText(doc, minPos, maxPos, pattern, caseSensitive, word, wordStart, searchFlags, length);
  }
  else {
    m_LastSearchEngine = DEELX_ENGINE;
    return m_DeelXEngine.FindText(doc, minPos, maxPos, pattern, caseSensitive, word, wordStart, searchFlags, length);
  }
}

const char* SciRegexSearch::SubstituteByPosition(Document* doc, const char* text, Sci::Position* length)
{
  if (m_LastSearchEngine == DEELX_ENGINE) 
  {
    return m_DeelXEngine.SubstituteByPosition(doc, text, length);
  }
  else {
    return m_TreEngine.SubstituteByPosition(doc, text, length);
  }
}


// ============================================================================
//   Some Helpers
// ============================================================================


/******************************************************************************
*
*  UnSlash functions
*  Mostly taken from SciTE, (c) Neil Hodgson, http://www.scintilla.org
*
/

/**
* Is the character an octal digit?
*/
static bool IsOctalDigit(char ch) {
  return ch >= '0' && ch <= '7';
}
// ----------------------------------------------------------------------------

/**
* If the character is an hexa digit, get its value.
*/
static int GetHexDigit(char ch) {
  if (ch >= '0' && ch <= '9') {
    return ch - '0';
  }
  if (ch >= 'A' && ch <= 'F') {
    return ch - 'A' + 10;
  }
  if (ch >= 'a' && ch <= 'f') {
    return ch - 'a' + 10;
  }
  return -1;
}
// ----------------------------------------------------------------------------


static void replaceAll(std::string& source,const std::string& from,const std::string& to)
{
  std::string newString;
  newString.reserve(source.length() * 2);  // avoids a few memory allocations

  std::string::size_type lastPos = 0;
  std::string::size_type findPos;

  while (std::string::npos != (findPos = source.find(from,lastPos))) {
    newString.append(source,lastPos,findPos - lastPos);
    newString += to;
    lastPos = findPos + from.length();
  }
  // Care for the rest after last occurrence
  newString += source.substr(lastPos);

  source.swap(newString);
}
// ----------------------------------------------------------------------------



static std::string& translateRegExpr(std::string& regExprStr, bool wholeWord, bool wordStart, int eolMode)
{
  std::string	tmpStr;

  if (wholeWord || wordStart) {      // push '\b' at the begin of regexpr
    tmpStr.push_back('\\');
    tmpStr.push_back('b');
    tmpStr.append(regExprStr);
    if (wholeWord) {               // push '\b' at the end of regexpr
      tmpStr.push_back('\\');
      tmpStr.push_back('b');
    }
    replaceAll(tmpStr, ".", R"(\w)");
  }
  else {
    tmpStr.append(regExprStr);
  }

  switch (eolMode) {
  case -1:
    // don't need this
    break;

  case SC_EOL_LF:
    // we are fine here
    break;

  case SC_EOL_CR:
    //TODO: don't know what to do here ...
    break;

  case SC_EOL_CRLF:
  {
    replaceAll(tmpStr, "$", R"(\r$)");
    replaceAll(tmpStr, R"(\\r$)", R"(\$)");
  }
  break;
  }

  std::swap(regExprStr, tmpStr);
  return regExprStr;
}
// ============================================================================


static std::string& convertReplExpr(std::string& replStr)
{
  std::string	tmpStr;
  for (size_t i = 0; i < replStr.length(); ++i) {
    char ch = replStr[i];
    if (ch == '\\') {
      ch = replStr[++i]; // next char
      if (ch >= '1' && ch <= '9') {
        // former behavior convenience: 
        // change "\\<n>" to deelx's group reference ($<n>)
        tmpStr.push_back('$');
      }
      switch (ch) {
        // check for escape seq:
      case 'a':
        tmpStr.push_back('\a');
        break;
      case 'b':
        tmpStr.push_back('\b');
        break;
      case 'f':
        tmpStr.push_back('\f');
        break;
      case 'n':
        tmpStr.push_back('\n');
        break;
      case 'r':
        tmpStr.push_back('\r');
        break;
      case 't':
        tmpStr.push_back('\t');
        break;
      case 'v':
        tmpStr.push_back('\v');
        break;
      case '\\':
        tmpStr.push_back('\\'); // preserve escd "\"
        tmpStr.push_back('\\'); 
        break;
      case 'x':
      case 'u':
        {
          bool bShort = (ch == 'x');
          char buf[8] = { '\0' };
          char *pch = buf;
          WCHAR val[2] = L"";
          int hex;
          val[0] = 0;
          ++i;
          hex = GetHexDigit(replStr[i]);
          if (hex >= 0) {
            ++i;
            val[0] = (WCHAR)hex;
            hex = GetHexDigit(replStr[i]);
            if (hex >= 0) {
              ++i;
              val[0] *= 16;
              val[0] += (WCHAR)hex;
              if (!bShort) {
                hex = GetHexDigit(replStr[i]);
                if (hex >= 0) {
                  ++i;
                  val[0] *= 16;
                  val[0] += (WCHAR)hex;
                  hex = GetHexDigit(replStr[i]);
                  if (hex >= 0) {
                    ++i;
                    val[0] *= 16;
                    val[0] += (WCHAR)hex;
                  }
                }
              }
            }
            if (val[0]) {
              val[1] = 0;
              WideCharToMultiByte(CP_UTF8, 0, val, -1, buf, ARRAYSIZE(val), NULL, NULL);
              tmpStr.push_back(*pch++);
              while (*pch)
                tmpStr.push_back(*pch++);
            }
            else
              tmpStr.push_back(ch); // unknown ctrl seq
          }
          else
            tmpStr.push_back(ch); // unknown ctrl seq
        }
        break;

      default:
        tmpStr.push_back(ch); // unknown ctrl seq
        break;
      }
    }
    else {
      tmpStr.push_back(ch);
    }
  } //for

  std::swap(replStr,tmpStr);
  return replStr;
}
// ============================================================================

#pragma warning( pop )

#endif //SCI_OWNREGEX
