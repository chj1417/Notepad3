/**
 * @file  RE2Interface.cxx
 * @brief integrate Google's RE2 regex searching for Scintilla library
 *              (Scintilla Lib is copyright 1998-2017 by Neil Hodgson <neilh@scintilla.org>)
 *
 * @autor Rainer Kottenhoff (RaPeHoff)
 */

#ifdef SCI_OWNREGEX

#include <stdlib.h>
#include <string>
#include <vector>

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
#include "re2/re2.h"   // Google's RE2 - Regular Expression Engine
// ---------------------------------------------------------------

using namespace Scintilla;
using namespace re2;
using namespace std;

// ---------------------------------------------------------------

typedef struct np3StringPiece_t {
  const char*	data;
  size_t      length;
} 
np3StringPiece_t;

const int MAX_GROUP_COUNT = 10;

// ---------------------------------------------------------------

class RE2Interface : public RegexSearchBase
{
public:

  explicit RE2Interface(CharClassify* charClassTable)
    : m_RegExprStrg()
    , m_MatchingGroup()
    , m_SubstitutionBuffer()
  {}

  virtual ~RE2Interface()
  {
    ClearInstanceData();
  }

  virtual long FindText(Document* doc,int minPos,int maxPos,const char* pattern,
                        bool caseSensitive,bool word,bool wordStart,int flags,int* length) override;

  virtual const char* SubstituteByPosition(Document* doc,const char* text,int* length) override;


private:

  inline void ClearInstanceData()
  {
    for (int i = 0; i < MAX_GROUP_COUNT; ++i)
    {
      m_MatchingGroup[i].data = nullptr;
      m_MatchingGroup[i].length = 0;
    }
  }

private:
  string            m_RegExprStrg;
  np3StringPiece_t  m_MatchingGroup[MAX_GROUP_COUNT];
  string            m_SubstitutionBuffer;
};
// ============================================================================


RegexSearchBase *Scintilla::CreateRegexSearch(CharClassify *charClassTable)
{
  return new RE2Interface(charClassTable);
}

// ============================================================================

/**
 * forward declaration of utility functions
 */
string& translateRegExpr(string& regExprStr,bool wholeWord,bool wordStart);
string& convertReplExpr(string& replStr);


// ============================================================================


/**
 * Find text in document, supporting both forward and backward
 * searches (just pass minPos > maxPos to do a backward search)
 * Has not been tested with backwards DBCS searches yet.
 */
long RE2Interface::FindText(Document* doc,int minPos,int maxPos,const char *pattern,
                            bool caseSensitive,bool word,bool wordStart,int searchFlags,int *length)
{
  
  // Range endpoints should not be inside DBCS characters, but just in case, move them.
  minPos = doc->MovePositionOutsideChar(minPos, 1, false);
  maxPos = doc->MovePositionOutsideChar(maxPos, 1, false);
  const bool findprevious = (minPos > maxPos);

  // determine document range to be searched for match

  int rangeBegin = (findprevious) ? maxPos : minPos;
  int rangeEnd = (findprevious) ? minPos : maxPos;
  int rangeLength = abs(maxPos - minPos);

#if 0
  Sci_Position linesTotal = doc->LinesTotal();
  Sci_Position fileLastPos = doc->Length();

  Sci_Position lineOfBegPos = doc->LineFromPosition(static_cast<Sci_Position>(rangeBegin));
  Sci_Position lineOfEndPos = doc->LineFromPosition(static_cast<Sci_Position>(rangeEnd));

  Sci_Position lineStartOfBegPos = doc->LineStart(lineOfBegPos);
  Sci_Position lineEndOfEndPos = doc->LineEnd(lineOfEndPos);

  size_t begMetaPos = m_RegExprStrg.find_first_of('^');
  bool bFoundBegMeta = (begMetaPos != string::npos) &&
    ((begMetaPos == 0) || (m_RegExprStrg.find_first_of('\\') != (begMetaPos - 1)));
  if (bFoundBegMeta) {
    if (lineStartOfBegPos != static_cast<Sci_Position>(rangeBegin)) {
      rangeBegin = (lineOfBegPos < linesTotal) ? doc->LineStart(lineOfBegPos + 1) : doc->LineEnd(linesTotal);
      rangeEnd = (rangeBegin <= rangeEnd) ? rangeEnd : rangeBegin;
    }
  }

  size_t endMetaPos = m_RegExprStrg.find_last_of('$');
  bool bFoundEndMeta = (endMetaPos != string::npos) &&
    ((endMetaPos == 0) || (m_RegExprStrg.find_last_of('\\') != (endMetaPos - 1)));
  if (bFoundEndMeta) {
    if (lineEndOfEndPos != static_cast<Sci_Position>(rangeEnd)) {
      rangeEnd = (0 < lineOfEndPos) ? doc->LineEnd(lineOfEndPos - 1) : 0;
      rangeBegin = (rangeBegin <= rangeEnd) ? rangeBegin : rangeEnd;
    }
  }
#endif

  const char* rangeBeginPtr = doc->RangePointer(rangeBegin, rangeLength);
  StringPiece inputStrgPiece(rangeBeginPtr, rangeLength);

  // prepare regex pattern to be matched 
  m_RegExprStrg = translateRegExpr(string(pattern), word, wordStart);

  RE2 re2Pattern(m_RegExprStrg, RE2::Quiet);  // default: UTF-8
  //RE2 re2Pattern(m_RegExprStrg, RE2::Latin1);

  if (!re2Pattern.ok()) {
    *length = 0;
    return -2L;
  }

  int nGrps = re2Pattern.NumberOfCapturingGroups();

  vector<StringPiece> strv(nGrps);
  vector<RE2::Arg>    argv(nGrps);
  vector<RE2::Arg*>   args(nGrps);

  for (int i = 0; i < nGrps; ++i) 
  {
    argv[i] = &(strv.data())[i];
    args[i] = &(argv.data())[i];
  }

  bool bMatch = RE2::PartialMatchN(inputStrgPiece, re2Pattern, args.data(), nGrps);

  ClearInstanceData();

  if (bMatch) {
    m_MatchingGroup[0].data = inputStrgPiece.data();
    m_MatchingGroup[0].length = inputStrgPiece.length();
    for (int i = 0; i < min(MAX_GROUP_COUNT,nGrps); ++i) 
    {
      m_MatchingGroup[i].data   = strv[i].data();
      m_MatchingGroup[i].length = strv[i].length();
    }
  }

  //NOTE: potential 64-bit-size issue at interface here:
  *length = static_cast<int>((bMatch) ? m_MatchingGroup[0].length : 0);
  return static_cast<long>((bMatch) ? (m_MatchingGroup[0].data - rangeBeginPtr) : -1L);

}
// ============================================================================


const char* RE2Interface::SubstituteByPosition(Document* doc, const char* text, int* length)
{

  // Like Replace, except that if the pattern matches, "rewrite"
  // is copied into "out" with substitutions.  The non-matching
  // portions of "text" are ignored.
  //
  // Returns true iff a match occurred and the extraction happened
  // successfully;  if no match occurs, the string is left unaffected.
  //
  // REQUIRES: "text" must not alias any part of "*out".
  //static bool Extract(const StringPiece &text,
  //  const RE2& pattern,
  //  const StringPiece &rewrite,
  //  string *out);



#if 0
  if (!m_Match.IsMatched() || (m_MatchPos < 0)) {
    *length = 0;
    return nullptr;
  }
  string sReplStrg = convertReplExpr(string(text,*length));

  //NOTE: potential 64-bit-size issue at interface here:
  const char* pString = doc->RangePointer(static_cast<int>(m_MatchPos),static_cast<int>(m_MatchLength));

  deelx::index_t resLength;
  ReleaseSubstitutionBuffer();
  m_SubstitutionBuffer = m_RegExpr.Replace(pString,m_MatchLength,sReplStrg.c_str(),
                                           static_cast<deelx::index_t>(sReplStrg.length()),resLength);

  //NOTE: potential 64-bit-size issue at interface here:
  *length = static_cast<int>(resLength);
#endif

  return m_SubstitutionBuffer.c_str();
}
// ============================================================================




// ============================================================================
//   Some Helpers
// ============================================================================


void replaceAll(string& source,const string& from,const string& to)
{
  string newString;
  newString.reserve(source.length() * 2);  // avoids a few memory allocations

  string::size_type lastPos = 0;
  string::size_type findPos;

  while (string::npos != (findPos = source.find(from,lastPos))) {
    newString.append(source,lastPos,findPos - lastPos);
    newString += to;
    lastPos = findPos + from.length();
  }
  // Care for the rest after last occurrence
  newString += source.substr(lastPos);

  source.swap(newString);
}
// ----------------------------------------------------------------------------



string& translateRegExpr(string& regExprStr,bool wholeWord,bool wordStart)
{
  string	tmpStr;

  if (wholeWord || wordStart) {      // push '\b' at the begin of regexpr
    tmpStr.push_back('\\');
    tmpStr.push_back('b');
    tmpStr.append(regExprStr);
    if (wholeWord) {               // push '\b' at the end of regexpr
      tmpStr.push_back('\\');
      tmpStr.push_back('b');
    }
    replaceAll(tmpStr,".",R"(\w)");
  }
  else {
    tmpStr.append(regExprStr);
  }
  swap(regExprStr,tmpStr);
  return regExprStr;
}
// ----------------------------------------------------------------------------



string& convertReplExpr(string& replStr)
{
  string	tmpStr;
  for (size_t i = 0; i < replStr.length(); ++i) {
    char ch = replStr[i];
    if (ch == '\\') {
      ch = replStr[++i]; // next char
      if (ch == '\\') {
        // skip 2nd backslash ("\\")
        if (i < replStr.length()) { ch = replStr[++i]; }
        else { break; }
      }
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
        tmpStr.push_back('\\');
        break;
      default:
        // unknown ctrl seq
        tmpStr.push_back(ch);
        break;
      }
    }
    else {
      tmpStr.push_back(ch);
    }
  } //for

  swap(replStr,tmpStr);
  return replStr;
}
// ============================================================================

#pragma warning( pop )

#endif //SCI_OWNREGEX
