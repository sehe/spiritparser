//#define BOOST_SPIRIT_DEBUG
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <iomanip>

namespace phx = boost::phoenix;
namespace qi  = boost::spirit::qi;

namespace AST {
    using Comment      = std::string;
    using CommentStack = std::vector<Comment>;

    struct CommentMixin {
        CommentStack comments;
    };

    struct Identifier : public std::string, public CommentMixin {};
    struct StringVal  : public std::string, public CommentMixin {};
    struct RefVal     : public std::string, public CommentMixin {};
    struct DoubleVal  : public CommentMixin { double number; };

    using AttributeValue = boost::variant<StringVal, DoubleVal, RefVal>;

    struct AttributeDef {
        Identifier name;
        AttributeValue value;
    };

    using AttributeDefs = std::vector<AttributeDef>;

    struct ObjectDef {
        Identifier obj, class_;
        AttributeDefs attributes;
    };

} // namespace AST

namespace AST { // SEHE: debug output helpers
    std::ostream& dump_comments(std::ostream& os, CommentStack const& cmts)
    {
        os << " CMTS[";
        bool first = true;
        for (Comment const& c : cmts)
            os << (std::exchange(first, false)? "":",") << std::quoted(c, '\'');
        return os << "]";
    }

    std::ostream& operator<<(std::ostream& os, DoubleVal const& o)
    {
        return dump_comments(os << o.number, o.comments);
    }

    template <typename T>
    static inline constexpr bool is_commentful_string =
            std::is_base_of_v<CommentMixin, std::decay_t<T>>
        and std::is_base_of_v<std::string, std::decay_t<T>>;

    template <typename T>
    static inline std::enable_if_t<is_commentful_string<T>, std::ostream&>
    operator<<(std::ostream& os, T const& o)
    {
        return dump_comments(os << std::quoted(o), o.comments);
    }

    static inline std::ostream& operator<<(std::ostream& os, ObjectDef const& o) {
        os << "ObjectDef {obj:" << o.obj
           << ", class_:" << o.class_;
        for (AttributeDef const& attr : o.attributes) {
            os << "\n    attr name:" << attr.name
               << "\n    attr value:" << attr.value;
        }
        return os << "\n}";
    }
} // namespace AST

namespace Parser {
    // Parser actions
    template <typename Iterator>
    struct ObjectDefParser : qi::grammar<Iterator, AST::ObjectDef()> {
        ObjectDefParser() : ObjectDefParser::base_type(start) {
            using namespace qi::labels;

            start = qi::skip(qi::copy(skipper))[objectdef >> qi::eoi];
            objectdef =
                (identifier >> identifier >> '(' >> attributedeflist >> ')');
            attributedeflist = -(attributedef % ',');
            attributedef     = (identifier >> ':' >> attributeval);

            identifier    = qi::lexeme[identimpl];
            identimpl     = ((qi::alpha | qi::char_('_')) >>
                            *(qi::alnum | qi::char_("_.")));

            attributeval  = refval | stringliteral | doubleval;
            stringliteral = qi::lexeme['"' >> *~qi::char_('"') >> '"'];
            doubleval     = qi::double_;
            refval        = "ref" >> ('(' >> -identifier >> ')');

            BOOST_SPIRIT_DEBUG_NODES(
                (objectdef)(attributedeflist)(attributedef)(attributedeflist)(
                    identifier)(attributeval)(stringliteral)(doubleval))

            auto collect = phx::function{[=](AST::CommentMixin& node) {
                node.comments.clear();
                node.comments.swap(m_comments);
            }}(_val);

            on_success(identifier,    collect);
            on_success(doubleval,     collect);
            on_success(stringliteral, collect);
            on_success(refval,        collect);
        }

      private:
        AST::CommentStack m_comments;

        struct Skip : qi::grammar<Iterator> {
            Skip(AST::CommentStack& comments) : Skip::base_type(start), m_comments(comments)
            {
                using namespace qi;
                auto push_comment = phx::push_back(phx::ref(m_comments), _1);

                single_line_comment = "//" >> *(char_ - eol) >> (eol | eoi);
                block_comment       = "/*" >> *(block_comment | char_ - "*/") >> "*/";
                start               = space |
                    as_string[raw[single_line_comment | block_comment]]
                             [push_comment];
            }

          private:
            AST::CommentStack& m_comments;

            qi::rule<Iterator> start;
            qi::rule<Iterator> block_comment, single_line_comment;
        } skipper{m_comments};

        template <typename Attr> using Production = qi::rule<Iterator, Attr(), Skip>;
        template <typename Attr> using Lexeme     = qi::rule<Iterator, Attr()>;
        Lexeme<AST::ObjectDef> start;
        //
        Production<AST::ObjectDef>      objectdef;
        Production<AST::AttributeDefs>  attributedeflist;
        Production<AST::AttributeDef>   attributedef;
        Production<AST::AttributeValue> attributeval;
        Production<AST::DoubleVal>      doubleval;
        Production<AST::RefVal>         refval;

        // lexemes
        Lexeme<AST::Identifier> identifier, identimpl;
        Lexeme<AST::StringVal> stringliteral;
    };
} // namespace Parser

BOOST_FUSION_ADAPT_STRUCT(AST::AttributeDef, name, value)
BOOST_FUSION_ADAPT_STRUCT(AST::ObjectDef, obj, class_, attributes)
BOOST_FUSION_ADAPT_STRUCT(AST::DoubleVal, number)

template <typename FwdIt>
bool parser(FwdIt it, FwdIt end, AST::ObjectDef& objects) {
    return qi::parse(it, end, Parser::ObjectDefParser<FwdIt>{}, objects);
}

int main()
{
    for (std::string const input : {(R"(
        /*c1*/
        myobj /*c2*/ sometype /*c3*/ ( /*c4*/ attr /*c5*/: /*c6*/ 10.0 )
    )")}) {
        auto it = input.cbegin(), end = input.end();
        AST::ObjectDef ast;

        // In ast, the comment /*c6*/ is present 2 times on node DoubleVal(10.0)
        if (parser(it, end, ast)) {
            std::cout << "Parsed: " << ast << "\n";
        } else {
            std::cout << "Parsing failed\n";
        }
    }
}
