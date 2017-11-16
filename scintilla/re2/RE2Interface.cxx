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

const int MAX_GROUP_COUNT = 10;

// ---------------------------------------------------------------

class RE2Interface : public RegexSearchBase
{
public:

  explicit RE2Interface(CharClassify* charClassTable)
    : m_RegExprStrg()
    , m_Options(RE2::Quiet)
    , m_MatchingGroup()
    , m_GroupCount(0)
    , m_MatchPos(-1)
    , m_MatchLength(0)
    , m_SubstitutionBuffer()
  {}

  virtual ~RE2Interface()
  {
    ClearInterface();
  }


  virtual long FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char* pattern,
                        bool caseSensitive, bool word, bool wordStart, int frFlags, Sci::Position* length) override;
    
  ///@return String with the substitutions, must remain valid until the next call or destruction
  virtual const char* SubstituteByPosition(Document* doc, const char* replace, Sci::Position* length) override;


private:

  inline void ClearInterface()
  {
    for (auto& grp : m_MatchingGroup) {
      grp.set(nullptr, 0);
    }
    m_GroupCount = 0;
  }

 void translateRegExpr(const string& regExprStr, bool wholeWord, bool wordStart, int eolMode);
 void setOptions(bool caseSensitive, bool word, bool wordStart, int frFlags);
 void setTextAnchors(Document* doc, Sci::Position begPos, Sci::Position endPos);
 
 string& convertReplExpr(string& replStr);


private:
  string            m_RegExprStrg;
  RE2::Options      m_Options;
  RE2::Anchor       m_Anchor;
  StringPiece       m_MatchingGroup[MAX_GROUP_COUNT];
  int               m_GroupCount;
  Sci::Position     m_MatchPos;
  Sci::Position     m_MatchLength;
  string            m_SubstitutionBuffer;
};
// ============================================================================


RegexSearchBase *Scintilla::CreateRegexSearch(CharClassify *charClassTable)
{
  return new RE2Interface(charClassTable);
}

// ============================================================================


/**
 * Find text in document, supporting both forward and backward
 * searches (just pass minPos > maxPos to do a backward search)
 * Has not been tested with backwards DBCS searches yet.
 */


long RE2Interface::FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char* pattern,
                            bool caseSensitive, bool word, bool wordStart, int frFlags, Sci::Position* length)
{
  Sci::Position docBeg = static_cast<Sci::Position>(0);
  Sci::Position docLen = static_cast<Sci::Position>(doc->Length());

  // Range endpoints should not be inside DBCS characters, but just in case, move them.
  minPos = doc->MovePositionOutsideChar(minPos, 1, false);
  maxPos = doc->MovePositionOutsideChar(maxPos, ((maxPos>0) ? -1 : 1), false);
  const bool findprevious = (minPos > maxPos);

  // determine document range to be searched for match
  Sci::Position rangeBegin = (findprevious) ? 0 : minPos;
  Sci::Position rangeEnd = (findprevious) ? minPos : maxPos;
  Sci::Position rangeLength = abs(maxPos - minPos);


  // ===  prepare regex pattern to be matched   ===

  ClearInterface();

  setOptions(caseSensitive, word, wordStart, frFlags);
  translateRegExpr(string(pattern), word, wordStart, doc->eolMode);
  setTextAnchors(doc, rangeBegin, rangeEnd);

  RE2 re2Pattern(m_RegExprStrg, m_Options);

  if (!re2Pattern.ok()) {
    *length = 0;
    return static_cast<long>(-2);
  }

  const char* docBeginPtr = doc->RangePointer(docBeg, docLen);
  StringPiece inputStrgPiece(docBeginPtr, static_cast<int>(docLen));

  m_GroupCount = min(1 + re2Pattern.NumberOfCapturingGroups(), MAX_GROUP_COUNT);

  bool bMatch = false;

  if (findprevious)  // search previous 
  {
    // search for last occurrence in range
    Sci::Position rBeg = rangeBegin;
    do {

      bMatch = re2Pattern.Match(inputStrgPiece, rBeg, rangeEnd, m_Anchor, m_MatchingGroup, m_GroupCount);

      if (bMatch) 
      {
        rangeBegin = rBeg;
        // prepare next match
        rBeg  = static_cast<Sci::Position>(m_MatchingGroup[0].data() - docBeginPtr) + 1;
      }
    } while (bMatch && (rBeg <= rangeEnd));
  }

  // --- find match in range and set region groups ---
  bMatch = re2Pattern.Match(inputStrgPiece, rangeBegin, rangeEnd, m_Anchor, m_MatchingGroup, m_GroupCount);

  m_GroupCount  = bMatch ? m_GroupCount : 0;
  m_MatchPos    = bMatch ? static_cast<Sci::Position>(m_MatchingGroup[0].data() - docBeginPtr) : static_cast<Sci::Position>(-1);
  m_MatchLength = bMatch ? static_cast<Sci::Position>(m_MatchingGroup[0].length()) : 0;

  //NOTE: potential 64-bit-size issue at interface here:
  *length = static_cast<int>(m_MatchLength);
  return static_cast<long>(m_MatchPos);
  
}
// ============================================================================



const char* RE2Interface::SubstituteByPosition(Document* doc, const char* replace, Sci::Position* length)
{
  if ((m_GroupCount == 0) || m_MatchPos < 0) {
    *length = 0;
    return nullptr;
  }

  RE2 re2Pattern(m_RegExprStrg, m_Options);

  if (!re2Pattern.ok()) {
    *length = 0;
    return nullptr;
  }

  const char* docBegin = doc->RangePointer(m_MatchPos, m_MatchLength);
  StringPiece docPiece(docBegin, m_MatchLength);

  string replStrg = convertReplExpr(string(replace, *length));

  RE2::Extract(docPiece, re2Pattern, StringPiece(replStrg), &m_SubstitutionBuffer);

  //NOTE: potential 64-bit-size issue at interface here:
  *length = static_cast<int>( m_SubstitutionBuffer.length());
  return m_SubstitutionBuffer.c_str();
 
}
// ============================================================================



void RE2Interface::setOptions(bool caseSensitive, bool word, bool wordStart, int frFlags)
{
  bool bPOSIX = (frFlags & SCFIND_POSIX);
  bool bWordBounds = (word || wordStart);

  //m_Options.set_utf8(true);                // (true), false -> Latin1

  m_Options.set_posix_syntax(bPOSIX);        // (false) restrict regexps to POSIX egrep syntax

  //m_Options.set_longest_match(false);      // (false) search for longest match, not first match

  m_Options.set_log_errors(false);           // (true) log syntax and execution errors to ERROR

  //m_Options.set_max_mem(kDefaultMaxMem);   // ((int64_t)kDefaultMaxMem) approx. max memory footprint of RE2
  //m_Options.set_literal(false);            // (false) interpret string as literal, not regexp
  //m_Options.set_never_nl(false);           // (false) never match \n, even if it is in regexp
  //m_Options.set_dot_nl(false);             // (false) dot matches everything including new line
  //m_Options.set_never_capture(false);      // (false)

  //   case-sensitive (regexp can override with (?i) unless in posix_syntax mode)
  m_Options.set_case_sensitive(caseSensitive); // (true)  match is case-sensitive
  
  //   The following options are only consulted when posix_syntax == true.
  //   (When posix_syntax == false these features are always enabled and cannot be turned off.)
  m_Options.set_perl_classes(bWordBounds);   // (false) allow Perl's \d \s \w \D \S \W
  m_Options.set_word_boundary(bWordBounds);  // (false) allow Perl's \b \B (word boundary and not)
  m_Options.set_one_line(false);             // (false) ^ and $ only match beginning and end of text

}
// ============================================================================


void RE2Interface::setTextAnchors(Document* doc, Sci::Position begPos, Sci::Position endPos)
{
  m_Anchor = RE2::UNANCHORED;   // No anchoring

                                /*
  Sci_Position linesTotal = doc->LinesTotal();
  Sci_Position fileLastPos = doc->Length();

  Sci_Position lineOfBegPos = doc->LineFromPosition(begPos);
  Sci_Position lineOfEndPos = doc->LineFromPosition(endPos);

  Sci_Position lineStartOfBegPos = doc->LineStart(lineOfBegPos);
  Sci_Position lineEndOfEndPos = doc->LineEnd(lineOfEndPos);

  if (lineStartOfBegPos == begPos) {
    m_Anchor = RE2::ANCHOR_START;         // Anchor at start only
    if (lineEndOfEndPos == endPos)
      m_Anchor = RE2::ANCHOR_BOTH;        // Anchor at start and end
  }
*/

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



void RE2Interface::translateRegExpr(const string& regExprStr, bool wholeWord, bool wordStart, int eolMode)
{
  string&	regExpr = m_RegExprStrg;

  regExpr.clear();

  if (wholeWord || wordStart) {      // push '\b' at the begin of regexpr
    regExpr.push_back('\\');
    regExpr.push_back('b');
    regExpr.append(regExprStr);
    if (wholeWord) {               // push '\b' at the end of regexpr
      regExpr.push_back('\\');
      regExpr.push_back('b');
    }
    replaceAll(regExpr, ".", R"(\w)");
  }
  else {
    regExpr.append(regExprStr);
  }

  switch (eolMode) {
  case SC_EOL_LF:
    // we are fine here
    break;

  case SC_EOL_CR:
    //TODO: don't know what to do here ...
    break;

  case SC_EOL_CRLF:
    {
      replaceAll(regExpr, "$", R"(\r$)");
      replaceAll(regExpr, R"(\\r$)", R"(\$)");
    }
    break;
  }
}
// ============================================================================



//TODO: maybe replaced byy : RE2::string QuoteMeta(const StringPiece& unquoted);


string& RE2Interface::convertReplExpr(string& replStr)
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
        // change "\\<n>" to deelx's group reference ($<n>)
        // tmpStr.push_back('$');
        tmpStr.push_back('\\');
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
