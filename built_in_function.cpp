#include "header.h"
#include "parse.h"
#include "built_in_funciton.h"
#include "parse_operation.h"
#include "function.h"
#include <boost/preprocessor/stringize.hpp>

namespace kp15{ namespace built_in_function{
	using namespace parse;

	//vectorからsequenceを生成する
	value_holder make_seq(const value_seq &args){
		const value_holder &arg(args[0]);
		value_holder result;
		switch(arg.type_id()){

#define MAKE_SEQ_VEC(VEC) \
	case value_type_seq_index<VEC>::value: \
		{ \
			result.assign(static_cast<value_seq*>(static_cast<VEC*>(arg.ptr())->value.ptr())); \
			static_cast<VEC*>(arg.ptr())->value.clear(); \
		} \
		break
			MAKE_SEQ_VEC(vector_container);
			MAKE_SEQ_VEC(c_vector_container);
#undef MAKE_SEQ_VEC

			default:
				throw(std::string("'make_seq' ") + arg.type_name() + "は引数になり得ません. ");
				break;
		}
		return result;
	}

	//一変数解析関数のケース部
#define ALALISYS_FN(fn_name) \
	case value_type_seq_index<mpz_class>::value: \
		result.assign(new mpf_class(kp15::fn_name(*static_cast<mpz_class*>(arg.ptr())))); \
		break; \
	case value_type_seq_index<mpf_class>::value: \
		result.assign(new mpf_class(kp15::fn_name(*static_cast<mpf_class*>(arg.ptr())))); \
		break; \
	case value_type_seq_index<mpq_class>::value: \
		result.assign(new mpf_class(kp15::fn_name(*static_cast<mpq_class*>(arg.ptr())))); \
		break; \

#define ADD_SUB_MUL_DIV(FN) \
	value_holder FN(const value_seq &args){ \
		value_holder result; \
		result = args[0].clone(); \
		for(std::size_t i = 1; i < args.size(); ++i){ \
			result = FN ## _value(result, args[i]); \
		} \
		return result; \
	}

	ADD_SUB_MUL_DIV(add);
	ADD_SUB_MUL_DIV(sub);
	ADD_SUB_MUL_DIV(mul);
	ADD_SUB_MUL_DIV(div);

	//一変数解析関数
#define UNARY_ANALISYS_FN(fn_name) \
	value_holder fn_name(const value_seq &args){ \
		const value_holder &arg(args[0]); \
		value_holder result; \
		switch(arg.type_id()){ \
			ALALISYS_FN(fn_name) \
			default: \
				throw(std::string("'") + BOOST_PP_STRINGIZE(fn_name) + "' " + arg.type_name() + "は引数になり得ません. "); \
				break; \
		} \
		return result; \
	}

	UNARY_ANALISYS_FN(abs);
	UNARY_ANALISYS_FN(exp);
	UNARY_ANALISYS_FN(log);
	UNARY_ANALISYS_FN(gamma);
	UNARY_ANALISYS_FN(sin);
	UNARY_ANALISYS_FN(cos);
	UNARY_ANALISYS_FN(tan);
	UNARY_ANALISYS_FN(sec);
	UNARY_ANALISYS_FN(csc);
	UNARY_ANALISYS_FN(cot);
	UNARY_ANALISYS_FN(asin);
	UNARY_ANALISYS_FN(acos);
	UNARY_ANALISYS_FN(atan);
	UNARY_ANALISYS_FN(sinh);
	UNARY_ANALISYS_FN(cosh);
	UNARY_ANALISYS_FN(tanh);
	UNARY_ANALISYS_FN(asinh);
	UNARY_ANALISYS_FN(acosh);
	UNARY_ANALISYS_FN(atanh);

	//ノルム
	value_holder norm(const value_seq &args){
		const value_holder &arg(args[0]);
		return norm_value(arg);
	}

	//行列式
	value_holder det(const value_seq &args){
		const value_holder &arg(args[0]);
		value_holder result;
		switch(arg.type_id()){
			case value_type_seq_index<matrix_container>::value:
				{
					value_holder b;
					b = arg.clone();
					value_seq *a = static_cast<value_seq*>(static_cast<matrix_container*>(b.ptr())->value.ptr());
					std::size_t n = a->size(), m = static_cast<value_seq*>((*a)[0].ptr())->size();
					if(n != m){
						throw(
							std::string("'det' ") +
							" 正方行列ではありません. " +
							boost::lexical_cast<std::string>(n) + "*" + boost::lexical_cast<std::string>(m) +
							". "
						);
					}
					for(std::size_t i = 0; i < n; ++i){
						for(std::size_t j = 0; j < n; ++j){
							if(i < j){
								value_holder buff;
								buff = div_value((*static_cast<value_seq*>((*a)[j].ptr()))[i], (*static_cast<value_seq*>((*a)[i].ptr()))[i]);
								for(std::size_t k = 0; k < n; ++k){
									(*static_cast<value_seq*>((*a)[j].ptr()))[k] = sub_value((*static_cast<value_seq*>((*a)[j].ptr()))[k], mul_value((*static_cast<value_seq*>((*a)[i].ptr()))[k], buff));
								}
							}
						}
					}
					result = (*static_cast<value_seq*>((*a)[0].ptr()))[0];
					for(std::size_t i = 1; i < n; ++i){
						result = mul_value(result, (*static_cast<value_seq*>((*a)[i].ptr()))[i]);
					}
				}
				break;

			default:
				throw(std::string("'det' ") + arg.type_name() + "は引数になり得ません. ");
				break;
		}
		return result;
	}

	//素因数分解
	value_holder primes(const value_seq &args){
		const value_holder &arg(args[0]);
		value_holder result;
		switch(arg.type_id()){
			case value_type_seq_index<mpz_class>::value:
				{
					value_seq *result_seq_ptr = new value_seq, &result_seq(*result_seq_ptr);
					mpz_class x = *static_cast<mpz_class*>(arg.ptr()), d, q;
					while(x >= 4 && x.get_ui() % 2 == 0){
						result_seq.push_back(value_holder());
						result_seq.back().assign(new mpz_class(2));
						x /= 2;
					}
					d = 3, q = x / d;
					while(q >= d){
						if(x % d == 0){
							result_seq.push_back(value_holder());
							result_seq.back().assign(new mpz_class(d));
							x = q;
						}else{
							d += 2;
						}
						q = x / d;
					}
					result_seq.push_back(value_holder());
					result_seq.back().assign(new mpz_class(x));
					result.assign(result_seq_ptr);
				}
				break;

			default:
				throw(std::string("'primes' ") + arg.type_name() + "は引数になり得ません. ");
				break;
		}
		return result;
	}

	//gcd
	value_holder gcd(const value_seq &args){
		static const std::string exception = "'gcd' 引数が整数値ではありません. ";
		value_holder result;
		if(args[0].type_id() != value_type_seq_index<mpz_class>::value){
			throw(exception);
		}
		result = args[0].clone();
		for(std::size_t i = 1; i < args.size(); ++i){
			if(args[i].type_id() != value_type_seq_index<mpz_class>::value){
				throw(exception);
			}
			mpz_class *ptr = new mpz_class;
			*ptr = kp15::gcd(*static_cast<mpz_class*>(result.ptr()), *static_cast<mpz_class*>(args[i].ptr()));
			result.assign(ptr);
		}
		return result;
	}

	//lcm
	value_holder lcm(const value_seq &args){
		static const std::string exception = "'lcm' 引数が整数値ではありません. ";
		value_holder result;
		if(args[0].type_id() != value_type_seq_index<mpz_class>::value){
			throw(exception);
		}
		result = args[0].clone();
		for(std::size_t i = 1; i < args.size(); ++i){
			if(args[i].type_id() != value_type_seq_index<mpz_class>::value){
				throw(exception);
			}
			mpz_class *ptr = new mpz_class;
			*ptr = kp15::lcm(*static_cast<mpz_class*>(result.ptr()), *static_cast<mpz_class*>(args[i].ptr()));
			result.assign(ptr);
		}
		return result;
	}

	//単位分数の列
	value_holder unit_frac(const value_seq &args){
		mpq_class f;
		const value_holder &arg(args[0]);
		switch(arg.type_id()){
			case value_type_seq_index<mpf_class>::value:
				f = abs(*static_cast<mpf_class*>(arg.ptr()));
				break;

			case value_type_seq_index<mpq_class>::value:
				f = abs(*static_cast<mpq_class*>(arg.ptr()));
				break;

			default:
				throw(std::string("'unit_frac' ") + arg.type_name() + "は引数になり得ません. ");
		}
		value_holder result;
		value_seq *seq;
		result.assign(seq = new value_seq);
		mpz_class m, n, q;
		m = f.get_num();
		n = f.get_den();
		mpz_t one;
		mpz_init(one);
		mpz_set_ui(one, 1);
		mpq_class *ptr;
		while(n % m != 0){
			q = n / m + 1;
			seq->push_back(value_holder());
			seq->back().assign(ptr = new mpq_class);
			mpq_set_num(ptr->__get_mp(), one);
			mpq_set_den(ptr->__get_mp(), q.get_mpz_t());
			m = m * q - n, n *= q;
		}
		seq->push_back(value_holder());
		seq->back().assign(ptr = new mpq_class(1 / (n / m)));
		mpq_set_num(ptr->__get_mp(), one);
		mpq_set_den(ptr->__get_mp(), mpz_class(n / m).get_mpz_t());
		return result;
	}

	//max
	value_holder max(const value_seq &args){
		value_holder result;
		result = args[0].clone();
		for(std::size_t i = 1; i < args.size(); ++i){
#define MAX_LT_CASE(L, R) \
	case value_type_seq_index<R>::value: \
		if(*static_cast<L*>(result.ptr()) < *static_cast<R*>(args[i].ptr())){ \
			result = args[i].clone(); \
		} \
		break

#define MAX_SWITCH_CASE(T) \
	case value_type_seq_index<T>::value: \
		switch(args[i].type_id()){ \
			MAX_LT_CASE(T, mpz_class); \
			MAX_LT_CASE(T, mpf_class); \
			MAX_LT_CASE(T, mpq_class); \
		default: \
			throw(std::string("'<' ") + result.type_name() + ", " + args[i].type_name() + ". 比較できません. "); \
		} \
		break

			switch(result.type_id()){
				MAX_SWITCH_CASE(mpz_class);
				MAX_SWITCH_CASE(mpf_class);
				MAX_SWITCH_CASE(mpq_class);

				default:
					throw(std::string("'<' ") + result.type_name() + ". 比較できません. ");
					break;
			}
		}
		return result;
	}

	//min
	value_holder min(const value_seq &args){
		value_holder result;
		result = args[0].clone();
		for(std::size_t i = 1; i < args.size(); ++i){
#define MIN_GT_CASE(L, R) \
	case value_type_seq_index<R>::value: \
		if(*static_cast<L*>(result.ptr()) > *static_cast<R*>(args[i].ptr())){ \
			result = args[i].clone(); \
		} \
		break

#define MIN_SWITCH_CASE(T) \
	case value_type_seq_index<T>::value: \
		switch(args[i].type_id()){ \
			MIN_GT_CASE(T, mpz_class); \
			MIN_GT_CASE(T, mpf_class); \
			MIN_GT_CASE(T, mpq_class); \
		default: \
			throw(std::string("'>' ") + result.type_name() + ", " + args[i].type_name() + ". 比較できません. "); \
		} \
		break

			switch(result.type_id()){
				MIN_SWITCH_CASE(mpz_class);
				MIN_SWITCH_CASE(mpf_class);
				MIN_SWITCH_CASE(mpq_class);

				default:
					throw(std::string("'>' ") + result.type_name() + ". 比較できません. ");
					break;
			}
		}
		return result;
	}

	//関数マップ本体
	fn_map_type fn_map;

	//built_in_funciton_mapを初期化する
	struct initializer_type{
		initializer_type(){
#define DECL_INIT_LIST(name, n) { fn_struct bf_struct; bf_struct.arg_num = n; bf_struct.fn = name; fn_map[BOOST_PP_STRINGIZE(name)] = bf_struct; }
			DECL_INIT_LIST(make_seq, 1);
			DECL_INIT_LIST(add, 0);
			DECL_INIT_LIST(sub, 0);
			DECL_INIT_LIST(mul, 0);
			DECL_INIT_LIST(div, 0);
			DECL_INIT_LIST(exp, 1);
			DECL_INIT_LIST(log, 1);
			DECL_INIT_LIST(gamma, 1);
			DECL_INIT_LIST(abs, 1);
			DECL_INIT_LIST(norm, 1);
			DECL_INIT_LIST(sin, 1);
			DECL_INIT_LIST(cos, 1);
			DECL_INIT_LIST(tan, 1);
			DECL_INIT_LIST(sec, 1);
			DECL_INIT_LIST(csc, 1);
			DECL_INIT_LIST(cot, 1);
			DECL_INIT_LIST(asin, 1);
			DECL_INIT_LIST(acos, 1);
			DECL_INIT_LIST(atan, 1);
			DECL_INIT_LIST(sinh, 1);
			DECL_INIT_LIST(cosh, 1);
			DECL_INIT_LIST(tanh, 1);
			DECL_INIT_LIST(asinh, 1);
			DECL_INIT_LIST(acosh, 1);
			DECL_INIT_LIST(atanh, 1);
			DECL_INIT_LIST(det, 1);
			DECL_INIT_LIST(primes, 1);
			DECL_INIT_LIST(unit_frac, 1);
			DECL_INIT_LIST(gcd, 0);
			DECL_INIT_LIST(lcm, 0);
			DECL_INIT_LIST(max, 0);
			DECL_INIT_LIST(min, 0);
#undef DECL_INIT_LIST
			fn_map.rehash(fn_map.size());
		}
	} initializer;
} }
