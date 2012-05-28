namespace kp15{ namespace lex{
	namespace spirit = boost::spirit;
	namespace qi = boost::spirit::qi;
	namespace ascii = boost::spirit::ascii;
	namespace phoenix = boost::phoenix;
	namespace lex = boost::spirit::lex;

	template<class Lexer>
	struct lexer : lex::lexer<Lexer>{
		lexer(){
			this->self.add
				("[ \t]+", parse::calc::token_null)("\n", parse::calc::token_eol)
				("([1-9][0-9]*)|0", parse::calc::token_integer)
				("([1-9][0-9]*|0)\\.[0-9]+", parse::calc::token_real)
				("\\(", parse::calc::token_l_pare)
				("\\)", parse::calc::token_r_pare)
				("\\[", parse::calc::token_l_bracket)
				("\\]", parse::calc::token_r_bracket)
				("\\{", parse::calc::token_l_brace)
				("\\}", parse::calc::token_r_brace)
				("<", parse::calc::token_l_broket)
				(">", parse::calc::token_r_broket)
				("=", parse::calc::token_equal)
				("\\+", parse::calc::token_add)
				("-", parse::calc::token_sub)
				("%", parse::calc::token_mod)
				("\\*", parse::calc::token_mul)
				("\\/", parse::calc::token_div)
				("\\*\\*", parse::calc::token_exprod)
				("\\.", parse::calc::token_dot)
				("\\/\\/", parse::calc::token_rational)
				("\\^", parse::calc::token_pow)
				(",", parse::calc::token_comma)
				("<-", parse::calc::token_l_allow)
				("->", parse::calc::token_r_allow)
				("!", parse::calc::token_exclamation)
				("integer|float|rational", parse::calc::token_built_in_type)
				("s[1-9][0-9]*", parse::calc::token_float_precision)
				("([a-z]|[A-Z]|_)([a-z]|[A-Z]|_|[0-9])*", parse::calc::token_id)
			;
		}
	};

	template<class Iterator>
	struct token_iterator{
		token_iterator(Iterator first_, Iterator last_, parse::calc::Token enum_value_) : first(first_), last(last_), enum_value(enum_value_){}
		Iterator first, last;
		parse::calc::Token enum_value;
	};

	template<class Iterator>
	struct token_manager{
		typedef bool result_type;
		typedef token_iterator<const char*> t_iterator;

		token_manager() : char_num(0), line_num(0){
			token_iterator_seq.clear();
		}

		template<class Token>
		bool operator ()(const Token &t){
			switch(t.id()){
				case parse::calc::token_eol:
					char_num = 0;
					++line_num;
					break;

				default:
					if(t.id() != parse::calc::token_null){
						token_iterator_seq.push_back(t_iterator(t.value().begin(), t.value().end(), static_cast<parse::calc::Token>(t.id())));
					}
					char_num += t.value().size();
					break;
			}
			return true;
		}

		std::size_t char_num, line_num;
		static std::vector<t_iterator> token_iterator_seq;
	};

	template<class Iterator>
	std::vector<typename token_manager<Iterator>::t_iterator> token_manager<Iterator>::token_iterator_seq;
} }