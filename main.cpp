#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/bind.hpp>
#include <boost/ref.hpp>
#include <boost/timer.hpp>
#include "header.h"
#include "function.h"

#include <fstream>

namespace kp15{ namespace parse{
#include "parser.hpp"
} }

#include "lex.hpp"
#include "parse.hpp"

namespace kp15{
void test_0(){
	return;
}

void test_1(){
	mp_exp_t exp;
	calc_log2(256);
	std::cout << mpf_log2.get_str(exp) << "\n";
	std::cout << mpf_sqrt2.get_str(exp) << "\n";
}

void test_2(){
	mpf_set_default_prec(4096);
	mpf_class a(2), b(0.5);
	std::cout << pow(a, b).get_str() << "\n";
	return;
}

void test_3(){
	mp_exp_t e;
	mpf_class a(0.25), b(2560);
	std::cout << a.r2_significand().get_str(e) << "\n";
	std::cout << a.r2_exp() << "\n";
	std::cout << b.r2_significand().get_str(e) << "\n";
	std::cout << b.r2_exp() << "\n";
}

void test_4(){
	mpf_class a, b, c;
	mp_exp_t e;

	unsigned int prec = 4096;

	a.set_prec(prec);
	b.set_prec(prec);
	calc_log2(prec);
	a = 42;

	boost::timer t;
	b = log(a);
	std::cout << t.elapsed() << "\n";
	std::cout << b.get_str(e) << "\n";

	return;
}

void test_5(){
	mpf_set_default_prec(512);
	mpf_class a(2), b(0.5), c;
	c = log(a) * b;
	std::cout << c.get_str() << "\n";
	std::cout << exp(c).get_str() << "\n";

	return;
}

void test_6(){
	typedef lex::token_manager<std::string::iterator> manager_type;
	lex::lexer<lex::lex::lexertl::lexer<> > lexer_functor;
	parse::semantic_action sa;

	for(;  ; ){
		std::string str;
		std::cout << "(^-^)? > ";
		if(!std::getline(std::cin, str) || str.size() == 0){
			std::cout << "(^-^)/~~\n";
			break;
		}

		manager_type manager;
		const char *first = str.c_str(), *last = first + str.size(), *iter = str.c_str();
		bool result = lex::lex::tokenize(iter, last, lexer_functor, boost::bind(manager, _1));
		if(!result || iter != last){
			std::cout << "(o_O)! > lex error " << (iter - first + 1) << "\n";
			continue;
		}

		parse::calc::Parser<parse::node*, parse::semantic_action> parser(sa);
		parse::node *value = 0;
		try{
			for(std::size_t i = 0; i < manager.token_iterator_seq.size(); ++i){
				manager_type::t_iterator &t_iter(manager.token_iterator_seq[i]);
				parse::node *node_ptr = 0;
				switch(t_iter.enum_value){
					case parse::calc::token_integer:
						node_ptr = new parse::integer(t_iter.first, t_iter.last);
						break;

					case parse::calc::token_real:
						node_ptr = new parse::real(t_iter.first, t_iter.last);
						break;

					case parse::calc::token_id:
					case parse::calc::token_built_in_type:
					case parse::calc::token_float_precision:
						node_ptr = new parse::identifier(t_iter.first, t_iter.last);
						break;
				}
				parser.post(t_iter.enum_value, node_ptr);
			}
			parser.post(parse::calc::token_eof, 0);
			if(!parser.accept(value)){ throw(std::string("parse error")); }
			std::cout << "(^-^)! > " << value->calc().get_str() << "\n";
			std::cout << "(^-^)! > ";
			value->put();
			std::cout << "\n";
		}catch(std::string message){
			std::cout << "(O_o)! > " << message << "\n";
		}
		delete value;
	}

	return;
}

void test_7(){
	mpz_class result = 0, p;
	for(int i = 1; i <= 2; ++i){
		p = 1;
		for(int j = 0; j < i; ++j){
			p *= 3;
		}
		result += p;
	}
	//std::ofstream out("ans_shiracha_probrem.txt");
	std::cout << result.get_str() << "\n";
}

typedef lex::token_manager<std::string::iterator> manager_type;
lex::lexer<lex::lex::lexertl::lexer<>> lexer_functor;
parse::semantic_action sa;
std::string output_str_buffer;

const char *calcstr(const char *input_str, int input_length, int &code){
	output_str_buffer.clear();
	manager_type manager;
	const char *first = input_str, *last = input_str + input_length;
	bool result = lex::lex::tokenize(first, last, lexer_functor, boost::bind(manager, _1));
	if(!result || first != last){
		output_str_buffer = "字句解析エラー : ";
		output_str_buffer += boost::lexical_cast<std::string>(static_cast<int>(first - input_str));
		output_str_buffer += "文字目. ";
		code = -1;
		return output_str_buffer.c_str();
	}

	parse::calc::Parser<parse::node*, parse::semantic_action> parser(sa);
	parse::node *value = 0;
	try{
		for(std::size_t i = 0; i < manager.token_iterator_seq.size(); ++i){
			manager_type::t_iterator &t_iter(manager.token_iterator_seq[i]);
			parse::node *node_ptr = 0;
			switch(t_iter.enum_value){
				case parse::calc::token_integer:
					node_ptr = new parse::integer(t_iter.first, t_iter.last);
					break;

				case parse::calc::token_real:
					node_ptr = new parse::real(t_iter.first, t_iter.last);
					break;

				case parse::calc::token_id:
				case parse::calc::token_built_in_type:
				case parse::calc::token_float_precision:
					node_ptr = new parse::identifier(t_iter.first, t_iter.last);
					break;
			}
			parser.post(t_iter.enum_value, node_ptr);
		}
		parser.post(parse::calc::token_eof, 0);
		if(!parser.accept(value)){ throw(std::string("構文解析エラー. ")); }
		output_str_buffer = value->calc().get_str();
	}catch(std::string message){
		delete value;
		output_str_buffer = message;
		code = -1;
		return output_str_buffer.c_str();
	}

	delete value;
	code = 0;
	return output_str_buffer.c_str();
}
} //namespace kp15

int main(int argc, char *argv[]){
	std::string str = argv[1];
	int code;
    kp15::calcstr(str.c_str(), str.size(), code);
    if(kp15::output_str_buffer.size() > 400){
        kp15::output_str_buffer = "出力文字数が大きすぎます";
    }
	std::cout << kp15::output_str_buffer;
	return 0;
}
