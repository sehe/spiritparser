//#define BOOST_SPIRIT_DEBUG
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <iomanip>
#include <deque>

namespace phx = boost::phoenix;
namespace qi  = boost::spirit::qi;

namespace AST {
    using Comment  = std::string;
    using Comments = std::vector<Comment>;

    struct CommentMixin {
        virtual ~CommentMixin() = default;
        Comments comments;
    };

    struct Identifier : std::string, CommentMixin { };

    enum class Kind { String, Ref };

    std::ostream& operator<<(std::ostream& os, Kind k) {
        switch(k) {
            case Kind::Ref:    return os << "Ref";
            case Kind::String: return os << "String";
        }
        return os << "?";
    }

    template <Kind kind, typename T>
    struct Tagged : T {
        using T::T;
        using T::operator=;

        friend std::ostream& operator<<(std::ostream& os, Tagged const& v) {
            return os << "(tagged:" << kind << ", " << static_cast<T const&>(v) << ")";
        }
    };

    template <typename T, typename... Mixin>
    struct Holder : Mixin... {
        T value;

        template <typename... Init>
        explicit Holder(Init&&... init) : value(std::forward<Init>(init)...) { }

        friend std::ostream& operator<<(std::ostream& os, Holder const& v) {
            return ((os << v.value) << ... << static_cast<Mixin const&>(v));
        }
    };

    using StringVal  = Holder<Tagged<Kind::String, std::string>>;
    using RefVal     = Holder<Tagged<Kind::Ref, std::string>>;
    using DoubleVal  = Holder<double>;

    using Variant        = boost::variant<StringVal, DoubleVal, RefVal>;
    using AttributeValue = Holder<Variant, CommentMixin>;

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

BOOST_FUSION_ADAPT_STRUCT(AST::AttributeDef, name, value)
BOOST_FUSION_ADAPT_STRUCT(AST::ObjectDef, obj, class_, attributes)
BOOST_FUSION_ADAPT_TPL_STRUCT((T), (AST::Holder)(T), value)
BOOST_FUSION_ADAPT_TPL_STRUCT((T)(Mixin), (AST::Holder)(T)(Mixin), value)

namespace AST { // SEHE: debug output helpers
    std::ostream& dump(std::ostream& os, Comments const& cmts)
    {
        bool first = true;
        for (Comment const& c : cmts)
            os << (std::exchange(first, false) ? "" : ",")
               << std::quoted(c, '\'');
        return os;
    }

    static std::ostream& operator<<(std::ostream& os, CommentMixin const& cm)
    {
        return dump(os << " CMTS[", cm.comments) << "]";
    }

    static std::ostream& operator<<(std::ostream& os, Identifier const& v) {
        return os << std::quoted(static_cast<std::string const&>(v))
                  << static_cast<CommentMixin const&>(v);
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
            attributedef     = identifier >> ':' >> attributeval;

            auto& s_ = skipper.m_stack;

#if TRUESTACK
            auto enter_ = phx::function{[&s_] { s_.emplace_back(); }};
            auto leave_ = phx::function{[&s_] { s_.pop_back(); }};

            auto collect_ = phx::function{[&s_](AST::CommentMixin& node) {
                // think of some useful semantics here?
                // in particular, when to clear the frame(s)
                for (auto& frame : s_)
                    node.comments.insert(node.comments.end(), frame.begin(),
                                         frame.end());
            }};
#else 
            [[maybe_unused]] auto constexpr specification = R"(
https://stackoverflow.com/questions/67531913/can-i-collect-attributes-from-my-skipper-parser/67535432?noredirect=1#comment119512956_67535432

Comments should never be duplicated. They should be collected until the
next AST node is created.Ie.the example:

    /*c1*/ obj /*c2/* type /*c3*/ ( /*c4*/ attr /*c5*/: /*c6*/ 10 /*c7*/)

should result in:

    ObjectDef{
        Identifier{"myobj", CommentMixin{{"/*c1*/"}}},
        Identifier{"sometype", CommentMixin{{"/*c2*/"}}},
        {AttributeDef{
            {Identifier{"attr",
                        CommentMixin{{"/*c3*/", "/*c4*"}}}},
            AttributeValue{DoubleVal{
                CommentMixin{{"/*c5*/", "/*c6*"}}, 10.0}}}}};


Comment /*c7*/ is lost (but would appear on the next Identifier whatever it
were)

– Jacob Lorensen 14 hours ago
)";
            auto enter_   = phx::function{[&s_] { /*s_.emplace_back();*/ }};
            auto leave_   = phx::function{[&s_] { /*s_.pop_back();*/ }};
            auto collect_ = phx::function{[&s_](AST::CommentMixin& node) {
                node.comments.swap(s_.back());
            }};
#endif

#define COMMENTFULLY(expr)                                                     \
    qi::eps[enter_()] >>                                                       \
        (((expr) >> qi::eps[collect_(_val)]) >> qi::eps[leave_()] |            \
         qi::eps[leave_()])

            identifier %= COMMENTFULLY((qi::alpha | qi::char_('_')) >>
                                       *(qi::alnum | qi::char_("_.")));

            attributeval %= COMMENTFULLY(refval | stringliteral | doubleval);
            stringliteral = qi::lexeme['"' >> *~qi::char_('"') >> '"'];
            doubleval     = qi::double_;
            refval        = "ref" >> ('(' >> -identifier >> ')');

            BOOST_SPIRIT_DEBUG_NODES(
                (objectdef)(attributedeflist)(attributedef)(attributedeflist)(
                    identifier)(attributeval)(stringliteral)(doubleval))

        }

      private:
        struct Skip : qi::grammar<Iterator> {
            Skip() : Skip::base_type(start), m_stack(1)
            {
                using namespace qi;
                auto push_comment =
                    phx::push_back(phx::back(phx::ref(m_stack)), _1);

                single_line_comment = "//" >> *(char_ - eol) >> (eol | eoi);
                block_comment       = "/*" >> *(block_comment | char_ - "*/") >> "*/";
                start               = space |
                    as_string[raw[single_line_comment | block_comment]]
                             [push_comment];
            }

            std::deque<AST::Comments> m_stack;
          private:
            qi::rule<Iterator> start;
            qi::rule<Iterator> block_comment, single_line_comment;
        } skipper;

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
        Lexeme<AST::Identifier> identifier;
        Lexeme<AST::StringVal> stringliteral;
    };
} // namespace Parser

template <typename FwdIt>
bool parser(FwdIt it, FwdIt end, AST::ObjectDef& objects) {
    return qi::parse(it, end, Parser::ObjectDefParser<FwdIt>{}, objects);
}

int main()
{
    for (std::string const input : {
             R"~~( /*c1*/ obj /*c2*/ type /*c3*/ (
                /*c4*/ attr /*c5*/: /*c6*/ 10 /*c7*/) )~~",
             R"~~( /*c1*/ myobj /*c2*/ sometype /*c3*/ (
            /*c4*/  attr  /*c5*/:  /*c6*/  10.0    /*c7*/,
            /*c8*/  attr2 /*c9*/:  /*c10*/ "LtUaE" /*c11*/,
            /*c12*/ attr3 /*c13*/: /*c14*/ ref    (something_else) /*c15*/
            ) )~~",
         }) {
        auto it = input.cbegin(), end = input.end();
        AST::ObjectDef ast;

        std::cout << "Input " << std::quoted(input) << "\n";
        // In ast, the comment /*c6*/ is present 2 times on node DoubleVal(10.0)
        if (parser(it, end, ast)) {
            std::cout << "Parsed: " << ast << "\n";
        } else {
            std::cout << "Parsing failed\n";
        }
    }
}
