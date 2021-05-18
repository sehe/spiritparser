
#include <iostream>

#define BOOST_SPIRIT_DEBUG

#include <boost/phoenix.hpp>
#include <boost/spirit/include/phoenix_function.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_action.hpp>
#include <boost/variant.hpp>

namespace phx = boost::phoenix;
namespace qi = boost::spirit::qi;

namespace AST {

struct CommentMixin {
  std::vector<std::string> comments;
};

struct Identifier : public std::string, public CommentMixin {};
struct StringVal : public std::string, public CommentMixin {};
struct RefVal : public std::string, public CommentMixin {};
struct DoubleVal : public CommentMixin {
  double number;
};

using AttributeValue = boost::variant<StringVal, DoubleVal, RefVal>;

struct AttributeDef {
  Identifier name;
  AttributeValue value;
};

struct ObjectDef {
  Identifier obj, class_;
  std::vector<AttributeDef> attributes;
};

} // namespace AST

namespace AST {

// Parser actions

phx::function const makeBlockComment =
    [](std::string &comment, const std::string &beg,
       std::vector<boost::variant<std::string, char>> const &cs,
       const std::string &end) {
      class visitor : public boost::static_visitor<std::string> {
      public:
        std::string operator()(char c) const { return std::string(1, c); }
        std::string operator()(const std::string &str) const { return str; }
      };

      comment.append(beg);
      for (const auto &c : cs) {
        comment.append(boost::apply_visitor(visitor(), c));
      }
      comment.append(end);
    };

phx::function collectComments = [](CommentMixin &node,
                                   std::vector<std::string> &comments) {
  std::copy(comments.cbegin(), comments.cend(),
            std::back_inserter(node.comments));
  comments.clear();
};

/**
 * Skipper parser that will handle space and nested comments
 */
template <typename Iterator> struct SkipperRules : qi::grammar<Iterator> {
  SkipperRules(std::vector<std::string> &comments)
      : SkipperRules::base_type(skipper), m_comments(comments) {
    single_line_comment =
        qi::lit("//") >> *(qi::char_ - qi::eol) >> (qi::eol | qi::eoi);
    block_comment =
        ((qi::string("/*") >> *(block_comment | qi::char_ - "*/")) >>
         qi::string("*/"))[makeBlockComment(qi::_val, qi::_1, qi::_2, qi::_3)];
    skipper =
        qi::space |
        single_line_comment[phx::push_back(phx::ref(m_comments), qi::_1)] |
        block_comment[phx::push_back(phx::ref(m_comments), qi::_1)];
  }
  std::vector<std::string> &m_comments;

  qi::rule<Iterator> skipper;
  qi::rule<Iterator, std::string()> block_comment;
  qi::rule<Iterator, std::string()> single_line_comment;
};

/**
 * Data file Grammar
 */
template <typename Iterator>
struct GrammarRules
    : qi::grammar<Iterator, ObjectDef(), SkipperRules<Iterator>> {
  GrammarRules(std::vector<std::string> &comments)
      : GrammarRules::base_type(objectdef), m_comments(comments) {
    objectdef = (identifier >> identifier >> '(' >> attributedeflist >> ')');
    attributedeflist = -(attributedef % ',');
    attributedef = (identifier >> ':' >> attributeval);

    identifier = qi::lexeme[identimpl];
    identimpl =
        ((qi::alpha | qi::char_('_')) >> *(qi::alnum | qi::char_("_.")));

    attributeval = refval | stringliteral | doubleval;
    stringliteral = qi::lexeme[qi::lit('"') >> *(qi::char_ - qi::char_('"')) >>
                               qi::lit('"')];
    doubleval = qi::double_;
    refval = qi::lit("ref") >> '(' >> -identifier >> ')';

    BOOST_SPIRIT_DEBUG_NODES((objectdef)(attributedeflist)(attributedef)(
        attributedeflist)(identifier)(attributeval)(stringliteral)(doubleval))

    on_success(identifier, collectComments(qi::_val, phx::ref(m_comments)));
    on_success(doubleval, collectComments(qi::_val, phx::ref(m_comments)));
    on_success(stringliteral, collectComments(qi::_val, phx::ref(m_comments)));
    on_success(refval, collectComments(qi::_val, phx::ref(m_comments)));
  }

  std::vector<std::string> &m_comments;

  qi::rule<Iterator, ObjectDef(), SkipperRules<Iterator>> objectdef;
  qi::rule<Iterator, std::vector<AttributeDef>(), SkipperRules<Iterator>>
      attributedeflist;
  qi::rule<Iterator, AttributeDef(), SkipperRules<Iterator>> attributedef;
  qi::rule<Iterator, Identifier()> identifier, identimpl;
  qi::rule<Iterator, AttributeValue(), SkipperRules<Iterator>> attributeval;
  qi::rule<Iterator, DoubleVal(), SkipperRules<Iterator>> doubleval;
  qi::rule<Iterator, RefVal(), SkipperRules<Iterator>> refval;
  qi::rule<Iterator, StringVal()> stringliteral;
};

} // namespace AST

BOOST_FUSION_ADAPT_STRUCT(AST::AttributeDef,
                          (AST::Identifier, name)(AST::AttributeValue, value));
BOOST_FUSION_ADAPT_STRUCT(AST::ObjectDef,
                          (AST::Identifier, obj)(AST::Identifier, class_)(
                              std::vector<AST::AttributeDef>, attributes));
BOOST_FUSION_ADAPT_STRUCT(AST::DoubleVal, (double, number));

template <typename InputIterator>
bool parser(InputIterator &it, InputIterator end, AST::ObjectDef &objects) {
  std::vector<std::string> commentStack;
  AST::GrammarRules<InputIterator> torParser(commentStack);
  AST::SkipperRules<InputIterator> torSkipper(commentStack);
  return qi::phrase_parse(it, end, torParser, torSkipper, objects);
}

int main(int, char *[]) {
  std::string input(R"V0G0N_POETRY(
/*c1*/
myobj /*c2*/ sometype /*c3*/ ( /*c4*/ attr /*c5*/: /*c6*/ 10.0 )
)V0G0N_POETRY");
  std::string::const_iterator it = input.cbegin();
  std::string::const_iterator end = input.end();
  AST::ObjectDef ast;
  const bool r = parser(it, end, ast);

  // In ast, the comment /*c6*/ is present 2 times on node DoubleVal(10.0)

  if (r && it == end) {
    std::cout << "Parsing succeeded\n";
  } else {
    std::cout << "Parsing failed\n";
  }
  std::cout << "  unparsed: ";
  std::copy(it, end, std::ostream_iterator<char>(std::cout));
  std::cout << "\n\n";
  std::cout << "Bye... :-) \n\n";
}
