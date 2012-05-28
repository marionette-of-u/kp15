#include "header.h"
#include "function.h"
#include "parse_operation.h"

namespace kp15{ namespace parse{
#define WRITE_OP_CASE(OP, RESULT, L, R) \
	case value_type_seq_index<R>::value: \
		result.assign(new RESULT(*static_cast<L*>(l.ptr()) OP *static_cast<R*>(r.ptr()))); \
		break

#define WRITE_OP_CASE_DIV(RESULT, L, R) \
	case value_type_seq_index<R>::value: \
		{ \
			if(*static_cast<R*>(r.ptr()) == 0){ throw(zero_div_error_message); } \
			result.assign(new RESULT(*static_cast<L*>(l.ptr()) / *static_cast<R*>(r.ptr()))); \
		} \
		break

#define WRITE_BIN_FN_CASE(FN, RESULT, L, R) \
	case value_type_seq_index<R>::value: \
		result.assign(new RESULT(FN(*static_cast<L*>(l.ptr()), *static_cast<R*>(r.ptr())))); \
		break

#define WRITE_POW_FN_CASE(FN, RESULT, L, R) \
	case value_type_seq_index<R>::value: \
	if(*static_cast<L*>(l.ptr()) == 0 && *static_cast<R*>(r.ptr()) == 0){ throw(exception_message); } \
		result.assign(new RESULT(FN(*static_cast<L*>(l.ptr()), *static_cast<R*>(r.ptr())))); \
		break

#define RATIONAL_FACTOR_PATTERN(L, R) \
	case value_type_seq_index<R>::value: \
		{ \
			const L &l_(*static_cast<L*>(l.ptr())); \
			const R &r_(*static_cast<R*>(r.ptr())); \
			if(r_ == 0){ throw(zero_div_error_message); } \
			result.assign(new mpq_class(l_ / r_)); \
		} \
		break

#define NEGATE_FACTOR_PATTERN(TYPE) \
	case value_type_seq_index<TYPE>::value: \
		result.assign(new TYPE(-(*static_cast<TYPE*>(f.ptr())))); \
		break

#define CAST_FACTOR_PATTERN(RESULT, TYPE) \
	case value_type_seq_index<TYPE>::value: \
		result.assign(new RESULT(*static_cast<TYPE*>(f.ptr()))); \
		break

#define OP_ERROR_COMBINATION_MESSAGE(OP, L, R) \
	{ \
		const std::string error_message(std::string("'") + L + OP + R + "' ââéZÇ≈Ç´Ç»Ç¢ëgÇ›çáÇÌÇπÇ≈Ç∑. "); \
		throw(error_message); \
	}

#define OP_ERROR_MESSAGE(OP, L, R) OP_ERROR_COMBINATION_MESSAGE(OP, L.type_name(), R.type_name())
#define OP_ERROR_MESSAGE_UNARY_FROTING(OP, F) OP_ERROR_COMBINATION_MESSAGE(OP, "", F.type_name())
#define OP_ERROR_MESSAGE_UNARY_POSTPOSING(OP, F) OP_ERROR_COMBINATION_MESSAGE(OP, F.type_name(), "")

#define VECTOR_OP(VEC, OP, FN) \
	case value_type_seq_index<VEC>::value: \
		{ \
			const value_seq &l_seq(*static_cast<value_seq*>(static_cast<VEC*>(l.ptr())->value.ptr())); \
			const value_seq &r_seq(*static_cast<value_seq*>(static_cast<VEC*>(r.ptr())->value.ptr())); \
			if(l_seq.size() != r_seq.size()){ throw(exception_vector_element_num(OP, l_seq.size(), r_seq.size())); } \
			value_seq *q = new value_seq; \
			q->resize(l_seq.size()); \
			for(std::size_t i = 0; i < l_seq.size(); ++i){ \
				(*q)[i] = FN(l_seq[i], r_seq[i]); \
			} \
			VEC *p = new VEC; \
			p->value.assign(q); \
			result.assign(p); \
		} \
		break

#define SCALAR_VECTOR_POW(VEC) \
	case value_type_seq_index<VEC>::value: \
		{ \
			const value_seq &r_seq(*static_cast<value_seq*>(static_cast<VEC*>(r.ptr())->value.ptr())); \
			value_seq *q = new value_seq; \
			q->resize(r_seq.size()); \
			for(std::size_t i = 0; i < r_seq.size(); ++i){ \
				(*q)[i] = pow_value(l, r_seq[i]); \
			} \
			VEC *p = new VEC; \
			p->value.assign(q); \
			result.assign(p); \
		} \
		break

#define MATRIX_OP(OP, FN) \
	case value_type_seq_index<matrix_container>::value: \
		{ \
			const value_seq &l_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(l.ptr())->value.ptr())), &r_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(r.ptr())->value.ptr())); \
			const value_seq *ll_seq = static_cast<value_seq*>(l_seq[0].ptr()), *rr_seq = static_cast<value_seq*>(r_seq[0].ptr()); \
			if( \
				l_seq.size() != r_seq.size() || \
				ll_seq->size() != rr_seq->size() \
			){ exception_matrix_element_num(OP, l_seq.size(), ll_seq->size(), r_seq.size(), rr_seq->size()); } \
			std::size_t n = l_seq.size(), m = ll_seq->size(); \
			value_seq *a, *b; \
			matrix_container *mat = new matrix_container; \
			mat->value.assign(a = new value_seq); \
			a->resize(n); \
			for(std::size_t i = 0; i < n; ++i){ \
				(*a)[i].assign(b = new value_seq); \
				b->resize(m); \
			} \
			for(std::size_t i = 0; i < n; ++i){ \
				ll_seq = static_cast<value_seq*>(l_seq[i].ptr()), rr_seq = static_cast<value_seq*>(r_seq[i].ptr()); \
				b = static_cast<value_seq*>((*a)[i].ptr()); \
				for(std::size_t j = 0; j < m; ++j){ \
					(*b)[j] = FN((*ll_seq)[j], (*rr_seq)[j]); \
				} \
			} \
			result.assign(mat); \
		} \
		break

#define SCALAR_MATRIX_POW \
	case value_type_seq_index<matrix_container>::value: \
		{ \
			const value_seq &r_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(r.ptr())->value.ptr())); \
			const value_seq *rr_seq = static_cast<value_seq*>(r_seq[0].ptr()); \
			std::size_t n = r_seq.size(), m = rr_seq->size(); \
			value_seq *a, *b; \
			matrix_container *mat = new matrix_container; \
			mat->value.assign(a = new value_seq); \
			a->resize(n); \
			for(std::size_t i = 0; i < n; ++i){ \
				(*a)[i].assign(b = new value_seq); \
				b->resize(m); \
			} \
			for(std::size_t i = 0; i < n; ++i){ \
				rr_seq = static_cast<value_seq*>(r_seq[i].ptr()); \
				b = static_cast<value_seq*>((*a)[i].ptr()); \
				for(std::size_t j = 0; j < m; ++j){ \
					(*b)[j] = pow_value(l, (*rr_seq)[j]); \
				} \
			} \
			result.assign(mat); \
		} \
		break

	std::string exception_vector_element_num(std::string msg, std::size_t n, std::size_t m){
		return std::string("'") + msg + "' vectorÇÃóvëfêîÇ™àÍívÇµÇ‹ÇπÇÒ. " + boost::lexical_cast<std::string>(n) + ", " + boost::lexical_cast<std::string>(m) + ". ";
	}

	std::string exception_matrix_element_num(std::string msg, std::size_t n0, std::size_t n1, std::size_t m0, std::size_t m1){
		return
			std::string("'") + msg + "' matrixÇÃóvëfêîÇ™àÍívÇµÇ‹ÇπÇÒ. " +
			boost::lexical_cast<std::string>(n0) + "*" + boost::lexical_cast<std::string>(n1) +
			", " +
			boost::lexical_cast<std::string>(m0) + "*" + boost::lexical_cast<std::string>(m1) +". ";
	}

	std::string exception_matrix_vector_element_num(std::string msg, std::size_t mat_n, std::size_t mat_m, std::size_t vec_n){
		return
			std::string("'") + msg + "' matrixÇ∆vectorÇÃóvëfêîÇ™àÍívÇµÇ‹ÇπÇÒ. " +
			boost::lexical_cast<std::string>(mat_n) + "*" + boost::lexical_cast<std::string>(mat_m) +
			", " +
			boost::lexical_cast<std::string>(vec_n);
	}

	value_holder add_value(const value_holder &l, const value_holder &r){
		value_holder result;
		switch(l.type_id()){
			case value_type_seq_index<mpz_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(+, mpz_class, mpz_class, mpz_class);
					WRITE_OP_CASE(+, mpf_class, mpz_class, mpf_class);
					WRITE_OP_CASE(+, mpq_class, mpz_class, mpq_class);

					default:
						OP_ERROR_MESSAGE("+", l, r);
				}
				break;

			case value_type_seq_index<mpf_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(+, mpf_class, mpf_class, mpz_class);
					WRITE_OP_CASE(+, mpf_class, mpf_class, mpf_class);
					WRITE_OP_CASE(+, mpf_class, mpf_class, mpq_class);

					default:
						OP_ERROR_MESSAGE("+", l, r);
				}
				break;

			case value_type_seq_index<mpq_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(+, mpq_class, mpq_class, mpz_class);
					WRITE_OP_CASE(+, mpf_class, mpq_class, mpf_class);
					WRITE_OP_CASE(+, mpq_class, mpq_class, mpq_class);

					default:
						OP_ERROR_MESSAGE("+", l, r);
				}
				break;

			case value_type_seq_index<vector_container>::value:
				switch(r.type_id()){
					VECTOR_OP(vector_container, "+", add_value);

					default:
						OP_ERROR_MESSAGE("+", l, r);
				}
				break;

			case value_type_seq_index<c_vector_container>::value:
				switch(r.type_id()){
					VECTOR_OP(c_vector_container, "+", add_value);

					default:
						OP_ERROR_MESSAGE("+", l, r);
				}
				break;

			case value_type_seq_index<matrix_container>::value:
				switch(r.type_id()){
					MATRIX_OP("+", add_value);

					default:
						OP_ERROR_MESSAGE("+", l, r);
				}
				break;

			default:
				OP_ERROR_MESSAGE("+", l, r);
		}
		return result;
	}

	value_holder sub_value(const value_holder &l, const value_holder &r){
		value_holder result;
		switch(l.type_id()){
			case value_type_seq_index<mpz_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(-, mpz_class, mpz_class, mpz_class);
					WRITE_OP_CASE(-, mpf_class, mpz_class, mpf_class);
					WRITE_OP_CASE(-, mpq_class, mpz_class, mpq_class);

					default:
						OP_ERROR_MESSAGE("-", l, r);
				}
				break;

			case value_type_seq_index<mpf_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(-, mpf_class, mpf_class, mpz_class);
					WRITE_OP_CASE(-, mpf_class, mpf_class, mpf_class);
					WRITE_OP_CASE(-, mpf_class, mpf_class, mpq_class);

					default:
						OP_ERROR_MESSAGE("-", l, r);
				}
				break;

			case value_type_seq_index<mpq_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(-, mpq_class, mpq_class, mpz_class);
					WRITE_OP_CASE(-, mpf_class, mpq_class, mpf_class);
					WRITE_OP_CASE(-, mpq_class, mpq_class, mpq_class);

					default:
						OP_ERROR_MESSAGE("-", l, r);
				}
				break;

			case value_type_seq_index<vector_container>::value:
				switch(r.type_id()){
					VECTOR_OP(vector_container, "-", sub_value);

					default:
						OP_ERROR_MESSAGE("-", l, r);
				}
				break;

			case value_type_seq_index<c_vector_container>::value:
				switch(r.type_id()){
					VECTOR_OP(c_vector_container, "-", sub_value);

					default:
						OP_ERROR_MESSAGE("-", l, r);
				}
				break;

			case value_type_seq_index<matrix_container>::value:
				switch(r.type_id()){
					MATRIX_OP("-", sub_value);

					default:
						OP_ERROR_MESSAGE("-", l, r);
				}
				break;

			default:
				OP_ERROR_MESSAGE("-", l, r);
		}
		return result;
	}

	value_holder mod_value(const value_holder &l, const value_holder &r){
		value_holder result;
		switch(l.type_id()){
			case value_type_seq_index<mpz_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(%, mpz_class, mpz_class, mpz_class);

					default:
						OP_ERROR_MESSAGE("%", l, r);
				}
				break;

			default:
				OP_ERROR_MESSAGE("%", l, r);
		}
		return result;
	}

	template<class Vec>
	value_holder mul_vector(const value_holder &scalar, const value_holder &vector){
		value_holder result;
		const value_seq &seq(*static_cast<value_seq*>(static_cast<Vec*>(vector.ptr())->value.ptr()));
		value_seq *q = new value_seq;
		q->resize(seq.size());
		for(std::size_t i = 0; i < seq.size(); ++i){
			(*q)[i] = mul_value(seq[i], scalar);
		}
		Vec *p = new Vec;
		p->value.assign(q);
		result.assign(p);
		return result;
	}

	value_holder mul_matrix(const value_holder &scalar, const value_holder &matrix){
		value_holder result;
		const value_seq &a_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(matrix.ptr())->value.ptr()));
		std::size_t n = a_seq.size(), m = static_cast<value_seq*>(a_seq[0].ptr())->size();
		value_seq *a, *b, *v_seq;
		matrix_container *mat = new matrix_container;
		mat->value.assign(a = new value_seq);
		a->resize(n);
		for(std::size_t i = 0; i < n; ++i){
			(*a)[i].assign(b = new value_seq);
			b->resize(m);
		}
		for(std::size_t i = 0; i < n; ++i){
			v_seq = static_cast<value_seq*>(a_seq[i].ptr());
			b = static_cast<value_seq*>((*a)[i].ptr());
			for(std::size_t j = 0; j < m; ++j){
				(*b)[j] = mul_value(scalar, (*v_seq)[j]);
			}
		}
		result.assign(mat);
		return result;
	}

	value_holder mul_matrix(const value_seq &l_seq, const value_seq &r_seq){
		value_holder result;
		if(static_cast<value_seq*>(l_seq[0].ptr())->size() != r_seq.size()){
			throw(
				std::string("'*' matrixÇÃóvëfêîÇ™àÍívÇµÇ‹ÇπÇÒ. ") +
				boost::lexical_cast<std::string>(l_seq.size()) + "*" + boost::lexical_cast<std::string>(static_cast<value_seq*>(l_seq[0].ptr())->size()) +
				", " +
				boost::lexical_cast<std::string>(r_seq.size()) + "*" + boost::lexical_cast<std::string>(static_cast<value_seq*>(r_seq[0].ptr())->size()) +
				". "
			);
		}
		std::size_t l = l_seq.size(), m = static_cast<value_seq*>(l_seq[0].ptr())->size(), n = static_cast<value_seq*>(r_seq[0].ptr())->size();
		value_seq *a, *b;
		matrix_container *mat = new matrix_container;
		mat->value.assign(a = new value_seq);
		a->resize(l);
		for(std::size_t i = 0; i < l; ++i){
			(*a)[i].assign(b = new value_seq);
			b->resize(n);
		}
		for(std::size_t i = 0; i < l; ++i){
			b = static_cast<value_seq*>((*a)[i].ptr());
			value_seq *lv_seq = static_cast<value_seq*>(l_seq[i].ptr());
			for(std::size_t j = 0; j < n; ++j){
				value_holder &sum((*b)[j]);
				sum.assign(new mpz_class(0));
				for(std::size_t k = 0; k < m; ++k){
					sum = add_value(sum, mul_value((*lv_seq)[k], (*static_cast<value_seq*>(r_seq[k].ptr()))[j]));
				}
			}
		}
		result.assign(mat);
		return result;
	}

	value_holder mul_value(const value_holder &l, const value_holder &r){
		value_holder result;
		switch(l.type_id()){
			case value_type_seq_index<mpz_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(*, mpz_class, mpz_class, mpz_class);
					WRITE_OP_CASE(*, mpf_class, mpz_class, mpf_class);
					WRITE_OP_CASE(*, mpq_class, mpz_class, mpq_class);
					
					case value_type_seq_index<vector_container>::value:
						result = mul_vector<vector_container>(l, r);
						break;

					case value_type_seq_index<c_vector_container>::value:
						result = mul_vector<c_vector_container>(l, r);
						break;

					case value_type_seq_index<matrix_container>::value:
						result = mul_matrix(l, r);
						break;

					default:
						OP_ERROR_MESSAGE("*", l, r);
				}
				break;

			case value_type_seq_index<mpf_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(*, mpf_class, mpf_class, mpz_class);
					WRITE_OP_CASE(*, mpf_class, mpf_class, mpf_class);
					WRITE_OP_CASE(*, mpf_class, mpf_class, mpq_class);

					case value_type_seq_index<vector_container>::value:
						result = mul_vector<vector_container>(l, r);
						break;

					case value_type_seq_index<c_vector_container>::value:
						result = mul_vector<c_vector_container>(l, r);
						break;

					case value_type_seq_index<matrix_container>::value:
						result = mul_matrix(l, r);
						break;

					default:
						OP_ERROR_MESSAGE("*", l, r);
				}
				break;

			case value_type_seq_index<mpq_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE(*, mpq_class, mpq_class, mpz_class);
					WRITE_OP_CASE(*, mpf_class, mpq_class, mpf_class);
					WRITE_OP_CASE(*, mpq_class, mpq_class, mpq_class);

					case value_type_seq_index<vector_container>::value:
						result = mul_vector<vector_container>(l, r);
						break;

					case value_type_seq_index<c_vector_container>::value:
						result = mul_vector<c_vector_container>(l, r);
						break;

					case value_type_seq_index<matrix_container>::value:
						result = mul_matrix(l, r);
						break;

					default:
						OP_ERROR_MESSAGE("*", l, r);
				}
				break;

			case value_type_seq_index<vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
					case value_type_seq_index<mpf_class>::value:
					case value_type_seq_index<mpq_class>::value:
						result = mul_vector<vector_container>(r, l);
						break;

					VECTOR_OP(vector_container, "*", mul_value);

					default:
						OP_ERROR_MESSAGE("*", l, r);
				}
				break;

			case value_type_seq_index<c_vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
					case value_type_seq_index<mpf_class>::value:
					case value_type_seq_index<mpq_class>::value:
						result = mul_vector<c_vector_container>(r, l);
						break;

					case value_type_seq_index<vector_container>::value:
						{
							const value_seq &l_seq(*static_cast<value_seq*>(static_cast<c_vector_container*>(l.ptr())->value.ptr()));
							const value_seq &r_seq(*static_cast<value_seq*>(static_cast<vector_container*>(r.ptr())->value.ptr()));
							value_seq *a, *b;
							matrix_container *mat = new matrix_container;
							std::size_t m = l_seq.size(), n = r_seq.size();
							mat->value.assign(a = new value_seq);
							a->resize(m);
							for(std::size_t i = 0; i < m; ++i){
								(*a)[i].assign(b = new value_seq);
								b->resize(n);
							}
							for(std::size_t i = 0; i < m; ++i){
								b = static_cast<value_seq*>((*a)[i].ptr());
								for(std::size_t j = 0; j < n; ++j){
									(*b)[j] = mul_value(l_seq[i], r_seq[j]);
								}
							}
							result.assign(mat);
						}
						break;
						
					VECTOR_OP(c_vector_container, "*", mul_value);

					default:
						OP_ERROR_MESSAGE("*", l, r);
				}
				break;

			case value_type_seq_index<matrix_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
					case value_type_seq_index<mpf_class>::value:
					case value_type_seq_index<mpq_class>::value:
						result = mul_matrix(r, l);
						break;

					case value_type_seq_index<vector_container>::value:
						{
							const value_seq &l_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(l.ptr())->value.ptr()));
							const value_seq &r_seq(*static_cast<value_seq*>(static_cast<vector_container*>(r.ptr())->value.ptr()));
							std::size_t ll_size = static_cast<value_seq*>(l_seq[0].ptr())->size();
							if(l_seq.size() != r_seq.size()){ exception_matrix_vector_element_num("*", l_seq.size(), ll_size, r_seq.size()); }
							value_seq *a, *b;
							matrix_container *mat = new matrix_container;
							mat->value.assign(a = new value_seq);
							a->resize(l_seq.size());
							for(std::size_t i = 0; i < l_seq.size(); ++i){
								(*a)[i].assign(b = new value_seq);
								b->resize(ll_size);
								for(std::size_t j = 0; j < ll_size; ++j){
									(*b)[j] = mul_value((*static_cast<value_seq*>(l_seq[i].ptr()))[j], r_seq[i]);
								}
							}
							result.assign(mat);
						}
						break;

					MATRIX_OP("*", mul_value);

					default:
						OP_ERROR_MESSAGE("*", l, r);
				}
				break;

			default:
				OP_ERROR_MESSAGE("*", l, r);
		}
		return result;
	}

	template<class Vec>
	value_holder div_vector(const value_holder &vector, const value_holder &scalar){
		value_holder result;
		const value_seq &seq(*static_cast<value_seq*>(static_cast<Vec*>(vector.ptr())->value.ptr()));
		value_seq *q = new value_seq;
		q->resize(seq.size());
		for(std::size_t i = 0; i < seq.size(); ++i){
			(*q)[i] = div_value(seq[i], scalar);
		}
		Vec *p = new Vec;
		p->value.assign(q);
		result.assign(p);
		return result;
	}

	template<class Vec>
	value_holder div_r_vector(const value_holder &scalar, const value_holder &vector){
		value_holder result;
		const value_seq &seq(*static_cast<value_seq*>(static_cast<Vec*>(vector.ptr())->value.ptr()));
		value_seq *q = new value_seq;
		q->resize(seq.size());
		for(std::size_t i = 0; i < seq.size(); ++i){
			(*q)[i] = mul_value(scalar, seq[i]);
		}
		Vec *p = new Vec;
		p->value.assign(q);
		result.assign(p);
		return result;
	}

	value_holder div_matrix(const value_holder &matrix, const value_holder &scalar){
		value_holder result;
		const value_seq &a_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(matrix.ptr())->value.ptr()));
		std::size_t n = a_seq.size(), m = static_cast<value_seq*>(a_seq[0].ptr())->size();
		value_seq *a, *b, *v_seq;
		matrix_container *mat = new matrix_container;
		mat->value.assign(a = new value_seq);
		a->resize(n);
		for(std::size_t i = 0; i < n; ++i){
			(*a)[i].assign(b = new value_seq);
			b->resize(m);
		}
		for(std::size_t i = 0; i < n; ++i){
			v_seq = static_cast<value_seq*>(a_seq[i].ptr());
			b = static_cast<value_seq*>((*a)[i].ptr());
			for(std::size_t j = 0; j < m; ++j){
				(*b)[j] = div_value((*v_seq)[j], scalar);
			}
		}
		result.assign(mat);
		return result;
	}

	value_holder div_r_matrix(const value_holder &scalar, const value_holder &matrix){
		value_holder result;
		const value_seq &a_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(matrix.ptr())->value.ptr()));
		std::size_t n = a_seq.size(), m = static_cast<value_seq*>(a_seq[0].ptr())->size();
		value_seq *a, *b, *v_seq;
		matrix_container *mat = new matrix_container;
		mat->value.assign(a = new value_seq);
		a->resize(n);
		for(std::size_t i = 0; i < n; ++i){
			(*a)[i].assign(b = new value_seq);
			b->resize(m);
		}
		for(std::size_t i = 0; i < n; ++i){
			v_seq = static_cast<value_seq*>(a_seq[i].ptr());
			b = static_cast<value_seq*>((*a)[i].ptr());
			for(std::size_t j = 0; j < m; ++j){
				(*b)[j] = div_value(scalar, (*v_seq)[j]);
			}
		}
		result.assign(mat);
		return result;
	}

	value_holder div_value(const value_holder &l, const value_holder &r){
		value_holder result;
		static const std::string zero_div_error_message("ââéZéq'/' 0èúéZÇ™î≠ê∂ÇµÇ‹ÇµÇΩ. ");
		switch(l.type_id()){
			case value_type_seq_index<mpz_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE_DIV(mpz_class, mpz_class, mpz_class);
					WRITE_OP_CASE_DIV(mpf_class, mpz_class, mpf_class);
					WRITE_OP_CASE_DIV(mpq_class, mpz_class, mpq_class);

					case value_type_seq_index<vector_container>::value:
						result = div_r_vector<vector_container>(l, r);
						break;

					case value_type_seq_index<c_vector_container>::value:
						result = div_r_vector<c_vector_container>(l, r);
						break;

					case value_type_seq_index<matrix_container>::value:
						result = div_r_matrix(l, r);
						break;

					default:
						OP_ERROR_MESSAGE("/", l, r);
				}
				break;

			case value_type_seq_index<mpf_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE_DIV(mpf_class, mpf_class, mpz_class);
					WRITE_OP_CASE_DIV(mpf_class, mpf_class, mpf_class);
					WRITE_OP_CASE_DIV(mpf_class, mpf_class, mpq_class);

					case value_type_seq_index<vector_container>::value:
						result = div_r_vector<vector_container>(l, r);
						break;

					case value_type_seq_index<c_vector_container>::value:
						result = div_r_vector<c_vector_container>(l, r);
						break;

					case value_type_seq_index<matrix_container>::value:
						result = div_r_matrix(l, r);
						break;

					default:
						OP_ERROR_MESSAGE("/", l, r);
				}
				break;

			case value_type_seq_index<mpq_class>::value:
				switch(r.type_id()){
					WRITE_OP_CASE_DIV(mpq_class, mpq_class, mpz_class);
					WRITE_OP_CASE_DIV(mpf_class, mpq_class, mpf_class);
					WRITE_OP_CASE_DIV(mpq_class, mpq_class, mpq_class);

					case value_type_seq_index<vector_container>::value:
						result = div_r_vector<vector_container>(l, r);
						break;

					case value_type_seq_index<c_vector_container>::value:
						result = div_r_vector<c_vector_container>(l, r);
						break;

					case value_type_seq_index<matrix_container>::value:
						result = div_r_matrix(l, r);
						break;

					default:
						OP_ERROR_MESSAGE("/", l, r);
				}
				break;

			case value_type_seq_index<vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
					case value_type_seq_index<mpf_class>::value:
					case value_type_seq_index<mpq_class>::value:
						result = div_vector<vector_container>(l, r);
						break;

					VECTOR_OP(vector_container, "/", div_value);

					default:
						OP_ERROR_MESSAGE("/", l, r);
				}
				break;

			case value_type_seq_index<c_vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
					case value_type_seq_index<mpf_class>::value:
					case value_type_seq_index<mpq_class>::value:
						result = div_vector<c_vector_container>(l, r);
						break;

					VECTOR_OP(c_vector_container, "/", div_value);

					default:
						OP_ERROR_MESSAGE("/", l, r);
				}
				break;

			case value_type_seq_index<matrix_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
					case value_type_seq_index<mpf_class>::value:
					case value_type_seq_index<mpq_class>::value:
						result = div_matrix(l, r);
						break;

					MATRIX_OP("/", div_value);

					default:
						OP_ERROR_MESSAGE("/", l, r);
				}
				break;

			default:
				OP_ERROR_MESSAGE("/", l, r);
		}
		return result;
	}

	template<class Vec>
	value_holder exprod_vector(const value_holder &l, const value_holder &r){
		value_holder result;
		const value_seq &l_(*static_cast<value_seq*>(static_cast<Vec*>(l.ptr())->value.ptr()));
		const value_seq &r_(*static_cast<value_seq*>(static_cast<Vec*>(r.ptr())->value.ptr()));
		value_seq *a, *b;
		matrix_container *mat = new matrix_container;
		mat->value.assign(a = new value_seq);
		a->resize(l_.size());
		for(std::size_t i = 0; i < l_.size(); ++i){
			(*a)[i].assign(b = new value_seq);
			b->resize(r_.size());
		}
		for(std::size_t i = 0; i < l_.size(); ++i){
			for(std::size_t j = 0; j < r_.size(); ++j){
				value_holder ab = mul_value(l_[i], r_[j]);
				(*static_cast<value_seq*>((*a)[i].ptr()))[j] = ab;
			}
		}
		result.assign(mat);
		return result;
	}

	value_holder exprod_value(const value_holder &l, const value_holder &r){
		value_holder result;
		switch(l.type_id()){
			case value_type_seq_index<vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<vector_container>::value:
						result = exprod_vector<vector_container>(l, r);
						break;

					default:
						result = mul_value(l, r);
				}
				break;

			case value_type_seq_index<c_vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<c_vector_container>::value:
						result = exprod_vector<c_vector_container>(l, r);
						break;

					default:
						result = mul_value(l, r);
				}
				break;

			default:
				result = mul_value(l, r);
		}
		return result;
	}

	template<class Vec>
	value_holder dot_vector(const value_holder &l, const value_holder &r){
		value_holder result;
		const value_seq &l_seq(*static_cast<value_seq*>(static_cast<Vec*>(l.ptr())->value.ptr()));
		const value_seq &r_seq(*static_cast<value_seq*>(static_cast<Vec*>(r.ptr())->value.ptr()));
		if(l_seq.size() != r_seq.size()){ throw(exception_vector_element_num("*", l_seq.size(), r_seq.size())); }
		result.assign(new mpz_class(0));
		for(std::size_t i = 0; i < l_seq.size(); ++i){
			value_holder w = mul_value(l_seq[i], r_seq[i]);
			value_holder x = add_value(result, w);
			result = x;
		}
		return result;
	}

	value_holder dot_value(const value_holder &l, const value_holder &r){
		value_holder result;
		switch(l.type_id()){
			case value_type_seq_index<vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<vector_container>::value:
						result = dot_vector<vector_container>(l, r);
						break;

					default:
						result = mul_value(l, r);
				}
				break;

			case value_type_seq_index<c_vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<c_vector_container>::value:
						result = dot_vector<c_vector_container>(l, r);
						break;

					default:
						result = mul_value(l, r);
				}
				break;

			case value_type_seq_index<matrix_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
					case value_type_seq_index<mpf_class>::value:
					case value_type_seq_index<mpq_class>::value:
						result = mul_matrix(r, l);
						break;

					case value_type_seq_index<c_vector_container>::value:
						{
							const value_seq &l_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(l.ptr())->value.ptr()));
							const value_seq &r_seq(*static_cast<value_seq*>(static_cast<c_vector_container*>(r.ptr())->value.ptr()));
							if(static_cast<value_seq*>(l_seq[0].ptr())->size() != r_seq.size()){
								throw(
									std::string("'*' matrix*vector^TÇÃóvëfêîÇ™àÍívÇµÇ‹ÇπÇÒ. ") +
									boost::lexical_cast<std::string>(l_seq.size()) + "*" + boost::lexical_cast<std::string>(static_cast<value_seq*>(l_seq[0].ptr())->size()) +
									", " +
									boost::lexical_cast<std::string>(r_seq.size()) +
									". "
								);
							}
							std::size_t c = l_seq.size(), r = r_seq.size();
							value_seq *a;
							c_vector_container *vec = new c_vector_container;
							vec->value.assign(a = new value_seq);
							a->resize(c);
							for(std::size_t i = 0; i < c; ++i){
								value_seq *lhs_seq = static_cast<value_seq*>(l_seq[i].ptr());
								value_holder &sum((*a)[i]);
								sum.assign(new mpz_class(0));
								for(std::size_t j = 0; j < r; ++j){
									sum = add_value(sum, mul_value((*lhs_seq)[j], r_seq[j]));
								}
							}
							result.assign(vec);
						}
						break;

					case value_type_seq_index<matrix_container>::value:
						{
							const value_seq &l_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(l.ptr())->value.ptr()));
							const value_seq &r_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(r.ptr())->value.ptr()));
							result = mul_matrix(l_seq, r_seq);
						}
						break;

					default:
						OP_ERROR_MESSAGE(".", l, r);
				}
				break;

			default:
				result = mul_value(l, r);
		}
		return result;
	}

	template<class Vec, class TVec>
	value_holder pow_t_vector(const value_holder &l){
		value_holder result;
		const value_seq &l_seq(*static_cast<value_seq*>(static_cast<Vec*>(l.ptr())->value.ptr()));
		value_seq *q = new value_seq;
		q->resize(l_seq.size());
		for(std::size_t i = 0; i < l_seq.size(); ++i){ (*q)[i] = l_seq[i]; }
		TVec *p = new TVec;
		p->value.assign(q);
		result.assign(p);
		return result;
	}

	value_holder inv_matrix(const value_holder &f){
		value_holder result;
		value_holder det;
		result = f.clone();
		det.assign(new mpz_class(1));
		value_seq *a = static_cast<value_seq*>(static_cast<matrix_container*>(result.ptr())->value.ptr());
		std::size_t n = a->size();
		for(std::size_t k = 0; k < n; ++k){
			value_holder t;
			t = (*static_cast<value_seq*>((*a)[k].ptr()))[k].clone();
			{
				det = mul_value(det, t);
			}
			for(std::size_t i = 0; i < n; ++i){
				value_holder &a_ki((*static_cast<value_seq*>((*a)[k].ptr()))[i]);
				a_ki = div_value(a_ki, t);
			}
			{
				value_holder one;
				one.assign(new mpz_class(1));
				(*static_cast<value_seq*>((*a)[k].ptr()))[k] = div_value(one, t);
			}
			for(std::size_t j = 0; j < n; ++j){
				if(j == k){ continue; }
				value_holder u;
				u = (*static_cast<value_seq*>((*a)[j].ptr()))[k].clone();
				for(std::size_t i = 0; i < n; ++i){
					if(i != k){
						(*static_cast<value_seq*>((*a)[j].ptr()))[i] = sub_value((*static_cast<value_seq*>((*a)[j].ptr()))[i], mul_value((*static_cast<value_seq*>((*a)[k].ptr()))[i], u));
					}else{
						(*static_cast<value_seq*>((*a)[j].ptr()))[i] = div_value(nagete_value(u), t);
					}
				}
			}
		}
		return result;
	}

	value_holder pow_value(const value_holder &l, const value_holder &r){
		value_holder result;
		std::string exception_message = "ââéZéq'^' ñ≥å¯Ç»ì¸óÕÇ≈Ç∑. x = 0, y = 0. ";
		switch(l.type_id()){
			case value_type_seq_index<mpz_class>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
						{
							const mpz_class &l_(*static_cast<mpz_class*>(l.ptr())), r_(*static_cast<mpz_class*>(r.ptr()));
							if(l_ == 0 && r_ == 0){
								throw(exception_message);
							}else if(r_ >= 0){
								result.assign(new mpz_class(pow_zzz(l_, r_)));
							}else{
								result.assign(new mpq_class(pow_qzz(l_, r_)));;
							}
						}
						break;

					WRITE_POW_FN_CASE(pow, mpf_class, mpz_class, mpf_class);
					WRITE_POW_FN_CASE(pow, mpf_class, mpz_class, mpq_class);

					SCALAR_VECTOR_POW(vector_container);
					SCALAR_VECTOR_POW(c_vector_container);
					SCALAR_MATRIX_POW;

					default:
						OP_ERROR_MESSAGE("^", l, r);
				}
				break;

			case value_type_seq_index<mpf_class>::value:
				switch(r.type_id()){
					WRITE_POW_FN_CASE(pow, mpf_class, mpf_class, mpz_class);
					WRITE_POW_FN_CASE(pow, mpf_class, mpf_class, mpf_class);
					WRITE_POW_FN_CASE(pow, mpf_class, mpf_class, mpq_class);

					SCALAR_VECTOR_POW(vector_container);
					SCALAR_VECTOR_POW(c_vector_container);
					SCALAR_MATRIX_POW;

					default:
						OP_ERROR_MESSAGE("^", l, r);
				}
				break;

			case value_type_seq_index<mpq_class>::value:
				switch(r.type_id()){
					WRITE_POW_FN_CASE(pow, mpf_class, mpq_class, mpz_class);
					WRITE_POW_FN_CASE(pow, mpf_class, mpq_class, mpf_class);
					WRITE_POW_FN_CASE(pow, mpf_class, mpq_class, mpq_class);
				
					SCALAR_VECTOR_POW(vector_container);
					SCALAR_VECTOR_POW(c_vector_container);
					SCALAR_MATRIX_POW;

					default:
						OP_ERROR_MESSAGE("^", l, r);
				}
				break;

			case value_type_seq_index<vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<vector_container>::value:
						{
							const value_seq &l_seq(*static_cast<value_seq*>(static_cast<vector_container*>(l.ptr())->value.ptr()));
							const value_seq &r_seq(*static_cast<value_seq*>(static_cast<vector_container*>(r.ptr())->value.ptr()));
							if(l_seq.size() != r_seq.size()){ throw(exception_vector_element_num("^", l_seq.size(), r_seq.size())); }
							if(l_seq.size() == 1){ throw(std::string("ââéZéq'^' vectorÇÃóvëfêîÇ™1Ç≈Ç∑. ")); }
							switch(l_seq.size()){
								case 2:
									{
										result = sub_value(mul_value(l_seq[0], r_seq[1]), mul_value(l_seq[1], r_seq[0]));
									}
									break;

								case 3:
									{
										value_seq *q = new value_seq;
										q->resize(3);
										(*q)[0] = sub_value(mul_value(l_seq[1], r_seq[2]), mul_value(l_seq[2], r_seq[1]));
										(*q)[1] = sub_value(mul_value(l_seq[2], r_seq[0]), mul_value(l_seq[0], r_seq[2]));
										(*q)[2] = sub_value(mul_value(l_seq[0], r_seq[1]), mul_value(l_seq[1], r_seq[0]));
										vector_container *p = new vector_container;
										p->value.assign(q);
										result.assign(p);
									}
									break;

								default:
									{
										std::size_t n = l_seq.size();
										value_seq *a, *b;
										matrix_container *mat = new matrix_container;
										mat->value.assign(a = new value_seq);
										a->resize(n);
										for(std::size_t i = 0; i < l_seq.size(); ++i){
											(*a)[i].assign(b = new value_seq);
											b->resize(n);
										}
										for(std::size_t i = 0; i < n; ++i){
											for(std::size_t j = 0; j < n; ++j){
												if(j == i){
													(*static_cast<value_seq*>((*a)[i].ptr()))[j].assign(new mpz_class(0));
												}else{
													(*static_cast<value_seq*>((*a)[i].ptr()))[j] = sub_value(mul_value(l_seq[i], r_seq[j]), mul_value(l_seq[j], r_seq[i]));
												}
											}
										}
										result.assign(mat);
									}
									break;
							}
						}
						break;

					case value_type_seq_index<identifier_container>::value:
						{
							identifier_container *identifier_ = static_cast<identifier_container*>(r.ptr());
							if(identifier_->str == "T"){
								result = pow_t_vector<vector_container, c_vector_container>(l);
							}else{
								OP_ERROR_COMBINATION_MESSAGE("^", partial_get_str(l.type_id(), l.ptr()), partial_get_str(r.type_id(), r.ptr()));
							}
						}
						break;

					default:
						OP_ERROR_MESSAGE("^", l, r);
				}
				break;

			case value_type_seq_index<c_vector_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<identifier_container>::value:
						{
							identifier_container *identifier_ = static_cast<identifier_container*>(r.ptr());
							if(identifier_->str == "T"){
								result = pow_t_vector<c_vector_container, vector_container>(l);
							}else{
								OP_ERROR_COMBINATION_MESSAGE("^", partial_get_str(l.type_id(), l.ptr()), partial_get_str(r.type_id(), r.ptr()));
							}
						}
						break;
				
					default:
						OP_ERROR_MESSAGE("^", l, r);
				}
				break;

			case value_type_seq_index<matrix_container>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
						{
							const value_seq &l_(*static_cast<value_seq*>(static_cast<matrix_container*>(l.ptr())->value.ptr()));
							if(l_.size() != static_cast<value_seq*>(l_[0].ptr())->size()){ throw(std::string("'^' ê≥ï˚çsóÒÇ≈ÇÕÇ†ÇËÇ‹ÇπÇÒ. ")); }
							const mpz_class &r_(*static_cast<mpz_class*>(r.ptr()));
							std::size_t n = l_.size();
							matrix_container *mat = new matrix_container;
							value_seq *a, *b;
							mat->value.assign(a = new value_seq);
							a->resize(n);
							for(std::size_t i = 0; i < n; ++i){
								(*a)[i].assign(b = new value_seq);
								b->resize(n);
							}
							if(r_ == 0){
								for(std::size_t i = 0; i < n; ++i){
									for(std::size_t j = 0; j < n; ++j){
										if(i == j){
											(*static_cast<value_seq*>((*a)[i].ptr()))[j].assign(new mpz_class(1));
										}else{
											(*static_cast<value_seq*>((*a)[i].ptr()))[j].assign(new mpz_class(0));
										}
									}
								}
								result.assign(mat);
							}else if(r_ == 1){
								result = l;
							}else if(r_ > 0){
								result = l.clone();
								for(mpz_class i = r_; i > 1; i -= 1){
									result = mul_matrix(*static_cast<value_seq*>(static_cast<matrix_container*>(result.ptr())->value.ptr()), l_);
								}
							}else if(r_ < 0){
								result = l;
								result = inv_matrix(result);
								{
									value_holder prod;
									prod = result.clone();
									for(mpz_class i = r_; i < -1; i += 1){
										prod = mul_matrix(*static_cast<value_seq*>(static_cast<matrix_container*>(prod.ptr())->value.ptr()), *static_cast<value_seq*>(static_cast<matrix_container*>(result.ptr())->value.ptr()));
									}
									result = prod;
								}
							}
						}
						break;

					case value_type_seq_index<identifier_container>::value:
						{
							identifier_container *identifier_ = static_cast<identifier_container*>(r.ptr());
							if(identifier_->str == "T"){
								result = transpose_value(l);
							}else{
								OP_ERROR_COMBINATION_MESSAGE("^", partial_get_str(l.type_id(), l.ptr()), partial_get_str(r.type_id(), r.ptr()));
							}
						}
						break;

					default:
						OP_ERROR_MESSAGE("^", l, r);
				}
				break;

			default:
				OP_ERROR_MESSAGE("^", l, r);
		}
		return result;
	}

	value_holder fact_value(const value_holder &f){
		value_holder result;
		switch(f.type_id()){
			case value_type_seq_index<mpz_class>::value:
				result.assign(new mpz_class);
				*static_cast<mpz_class*>(result.ptr()) = fact_zz(*static_cast<mpz_class*>(f.ptr()));
				break;

			case value_type_seq_index<mpf_class>::value:
				result.assign(new mpf_class);
				*static_cast<mpf_class*>(result.ptr()) = fact_ff(*static_cast<mpf_class*>(f.ptr()));
				break;

			case value_type_seq_index<mpq_class>::value:
				result.assign(new mpf_class);
				*static_cast<mpf_class*>(result.ptr()) = fact_ff(*static_cast<mpq_class*>(f.ptr()));
				break;

			default:
				OP_ERROR_MESSAGE_UNARY_POSTPOSING("-", f);
		}
		return result;
	}

	value_holder rational_value(const value_holder &l, const value_holder &r){
		static const std::string zero_div_error_message("ââéZéq'//' 0èúéZÇ™î≠ê∂ÇµÇ‹ÇµÇΩ. ");
		value_holder result;
		switch(l.type_id()){
			case value_type_seq_index<mpz_class>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
						{
							const mpz_class &l_(*static_cast<mpz_class*>(l.ptr())), r_(*static_cast<mpz_class*>(r.ptr()));
							if(r_ == 0){ throw(zero_div_error_message); }
							mpq_class *q = new mpq_class;
							mpq_set_num(q->__get_mp(), l_.__get_mp());
							mpq_set_den(q->__get_mp(), r_.__get_mp());
							result.assign(q);
						}
						break;

					RATIONAL_FACTOR_PATTERN(mpz_class, mpf_class);
					RATIONAL_FACTOR_PATTERN(mpz_class, mpq_class);

					default:
						throw(std::string("å^") + r.type_name() + std::string("ÇÕ'//'ÇÃâEï”Ç…Ç»ÇËìæÇ‹ÇπÇÒ. "));
				}
				break;

			case value_type_seq_index<mpf_class>::value:
				switch(r.type_id()){
					RATIONAL_FACTOR_PATTERN(mpf_class, mpz_class);
					RATIONAL_FACTOR_PATTERN(mpf_class, mpf_class);
					RATIONAL_FACTOR_PATTERN(mpf_class, mpq_class);

					default:
						throw(std::string("å^") + r.type_name() + std::string("ÇÕ'//'ÇÃâEï”Ç…Ç»ÇËìæÇ‹ÇπÇÒ. "));
				}
				break;

			case value_type_seq_index<mpq_class>::value:
				switch(r.type_id()){
					case value_type_seq_index<mpz_class>::value:
						{
							const mpq_class &l_(*static_cast<mpq_class*>(l.ptr()));
							const mpz_class &r_(*static_cast<mpz_class*>(r.ptr()));
							if(r_ == 0){ throw(zero_div_error_message); }
							mpq_class *q = new mpq_class;
							mpq_set_num(q->__get_mp(), &l_.__get_mp()->_mp_num);
							mpz_mul(&q->__get_mp()->_mp_den, &l_.__get_mp()->_mp_den, r_.__get_mp());
							result.assign(q);
						}
						break;

					RATIONAL_FACTOR_PATTERN(mpq_class, mpf_class);
					RATIONAL_FACTOR_PATTERN(mpq_class, mpq_class);

					default:
						throw(std::string("å^") + r.type_name() + std::string("ÇÕ'//'ÇÃâEï”Ç…Ç»ÇËìæÇ‹ÇπÇÒ. "));
				}
				break;

			default:
				throw(std::string("å^") + l.type_name() + std::string("ÇÕ'//'ÇÃç∂ï”Ç…Ç»ÇËìæÇ‹ÇπÇÒ. "));
		}
		return result;
	}

	template<class Vec>
	value_holder negete_vector(const value_holder &f){
		value_holder result;
		const value_seq &f_seq(*static_cast<value_seq*>(static_cast<Vec*>(f.ptr())->value.ptr()));
		value_seq *g_seq = new value_seq;
		g_seq->resize(f_seq.size());
		for(std::size_t i = 0; i < f_seq.size(); ++i){ (*g_seq)[i] = nagete_value(f_seq[i]); }
		Vec *v = new Vec;
		v->value.assign(g_seq);
		result.assign(v);
		return result;
	}

	value_holder nagete_value(const value_holder &f){
		value_holder result;
		switch(f.type_id()){
			NEGATE_FACTOR_PATTERN(mpz_class);
			NEGATE_FACTOR_PATTERN(mpf_class);
			NEGATE_FACTOR_PATTERN(mpq_class);

		case value_type_seq_index<vector_container>::value:
			result = negete_vector<vector_container>(f);
			break;

		case value_type_seq_index<c_vector_container>::value:
			result = negete_vector<c_vector_container>(f);
			break;

		case value_type_seq_index<matrix_container>::value:
			{
				const value_seq &a_seq(*static_cast<value_seq*>(static_cast<matrix_container*>(f.ptr())->value.ptr()));
				std::size_t n = a_seq.size(), m = static_cast<value_seq*>(a_seq[0].ptr())->size();
				value_seq *a, *b, *v_seq;
				matrix_container *mat = new matrix_container;
				mat->value.assign(a = new value_seq);
				a->resize(n);
				for(std::size_t i = 0; i < n; ++i){
					(*a)[i].assign(b = new value_seq);
					b->resize(m);
				}
				for(std::size_t i = 0; i < n; ++i){
					v_seq = static_cast<value_seq*>(a_seq[i].ptr());
					b = static_cast<value_seq*>((*a)[i].ptr());
					for(std::size_t j = 0; j < m; ++j){
						(*b)[j] = nagete_value((*v_seq)[j]);
					}
				}
				result.assign(mat);
			}
			break;

			default:
				OP_ERROR_MESSAGE_UNARY_FROTING("-", f);
		}
		return result;
	}

	value_holder cast_value(const value_holder &f, const std::string &cast_str){
		value_holder result;
		static const std::string integer_("integer"), float_("float"), rational_("rational");
		if(cast_str == integer_){
			switch(f.type_id()){
				CAST_FACTOR_PATTERN(mpz_class, mpz_class);
				CAST_FACTOR_PATTERN(mpz_class, mpf_class);
				CAST_FACTOR_PATTERN(mpz_class, mpq_class);

				default:
					throw(std::string("å^") + f.type_name() + std::string(" ïsê≥Ç»") + integer_ + std::string("ÉLÉÉÉXÉgÇ≈Ç∑. "));
			}
		}else if(cast_str == float_){
			switch(f.type_id()){
				CAST_FACTOR_PATTERN(mpf_class, mpz_class);
				CAST_FACTOR_PATTERN(mpf_class, mpf_class);
				CAST_FACTOR_PATTERN(mpf_class, mpq_class);

				default:
					throw(std::string("å^") + f.type_name() + std::string(" ïsê≥Ç»") + float_ + std::string("ÉLÉÉÉXÉgÇ≈Ç∑. "));
			}
		}else if(cast_str == rational_){
			switch(f.type_id()){
				CAST_FACTOR_PATTERN(mpq_class, mpz_class);
				CAST_FACTOR_PATTERN(mpq_class, mpf_class);
				CAST_FACTOR_PATTERN(mpq_class, mpq_class);

				default:
					throw(std::string("å^") + f.type_name() + std::string(" ïsê≥Ç»") + rational_ + std::string("ÉLÉÉÉXÉgÇ≈Ç∑. "));
			}
		}
		return result;
	}

	value_holder precision_cast_value(const value_holder &f){
		value_holder result;
		switch(f.type_id()){
			CAST_FACTOR_PATTERN(mpf_class, mpz_class);
			CAST_FACTOR_PATTERN(mpf_class, mpq_class);
			CAST_FACTOR_PATTERN(mpf_class, mpf_class);

			default:
				throw(std::string("å^") + f.type_name() + std::string(" ïsê≥Ç»floatÉLÉÉÉXÉgÇ≈Ç∑. "));
		}
		return result;
	}

	value_holder norm_value(const value_holder &f){
		value_holder result;
#define NORM_VEC_CASE(VEC) \
	case value_type_seq_index<VEC>::value: \
		{ \
			value_seq &seq(*static_cast<value_seq*>(static_cast<VEC*>(f.ptr())->value.ptr())); \
			result = mul_value(seq[0], seq[0]); \
			for(std::size_t i = 1; i < seq.size(); ++i){ \
				result = add_value(result, mul_value(seq[i], seq[i])); \
			} \
			{ \
				value_holder half; \
				half.assign(new mpf_class(0.5)); \
				result = pow_value(result, half); \
			} \
		} \
		break
		switch(f.type_id()){
			NORM_VEC_CASE(vector_container);
			NORM_VEC_CASE(c_vector_container);
		
		default:
			throw(std::string("'norm' ") + f.type_name() + "ÇÕà¯êîÇ…Ç»ÇËìæÇ‹ÇπÇÒ. ");
		}
		return result;
	}

	value_holder transpose_value(const value_holder &f){
		value_holder result;
		if(f.type_id() == value_type_seq_index<matrix_container>::value){
			value_seq *l_seq = static_cast<value_seq*>(static_cast<matrix_container*>(f.ptr())->value.ptr());
			std::size_t m = static_cast<value_seq*>((*l_seq)[0].ptr())->size(), n = l_seq->size();
			value_seq *a, *b;
			matrix_container *mat = new matrix_container;
			mat->value.assign(a = new value_seq);
			a->resize(m);
			for(std::size_t i = 0; i < m; ++i){
				(*a)[i].assign(b = new value_seq);
				b->resize(n);
			}
			for(std::size_t i = 0; i < n; ++i){
				value_seq *ll_seq = static_cast<value_seq*>((*l_seq)[i].ptr());
				for(std::size_t j = 0; j < m; ++j){
					(*static_cast<value_seq*>((*static_cast<value_seq*>(mat->value.ptr()))[j].ptr()))[i] = (*ll_seq)[j];
				}
			}
			result.assign(mat);
		}else{
			throw(std::string("'transpose' ") + f.type_name() + "Çì]íuÇ≈Ç´Ç‹ÇπÇÒ. ");
		}
		return result;
	}

	bool lt_value(const value_holder &l, const value_holder &r){
#define LT_CASE(L, R) \
	case value_type_seq_index<R>::value: \
		return *static_cast<L*>(l.ptr()) < *static_cast<R*>(r.ptr());

#define LT_SWITCH_CASE(T) \
	case value_type_seq_index<T>::value: \
		switch(r.type_id()){ \
			LT_CASE(T, mpz_class); \
			LT_CASE(T, mpf_class); \
			LT_CASE(T, mpq_class); \
		default: \
			throw(std::string("'<' ") + l.type_name() + ", " + r.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. "); \
		} \
		break

		switch(l.type_id()){
			LT_SWITCH_CASE(mpz_class);
			LT_SWITCH_CASE(mpf_class);
			LT_SWITCH_CASE(mpq_class);

			default:
				throw(std::string("'<' ") + l.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. ");
				break;
		}
		return false;
	}

	bool gt_value(const value_holder &l, const value_holder &r){
#define GT_CASE(L, R) \
	case value_type_seq_index<R>::value: \
		return *static_cast<L*>(l.ptr()) > *static_cast<R*>(r.ptr());

#define GT_SWITCH_CASE(T) \
	case value_type_seq_index<T>::value: \
		switch(r.type_id()){ \
			GT_CASE(T, mpz_class); \
			GT_CASE(T, mpf_class); \
			GT_CASE(T, mpq_class); \
		default: \
			throw(std::string("'>' ") + l.type_name() + ", " + r.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. "); \
		} \
		break

		switch(l.type_id()){
			GT_SWITCH_CASE(mpz_class);
			GT_SWITCH_CASE(mpf_class);
			GT_SWITCH_CASE(mpq_class);

			default:
				throw(std::string("'>' ") + l.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. ");
				break;
		}
		return false;
	}

	bool eq_value(const value_holder &l, const value_holder &r){
#define EQ_CASE(L, R) \
	case value_type_seq_index<R>::value: \
		return *static_cast<L*>(l.ptr()) == *static_cast<R*>(r.ptr());

#define EQ_SWITCH_CASE(T) \
	case value_type_seq_index<T>::value: \
		switch(r.type_id()){ \
			EQ_CASE(T, mpz_class); \
			EQ_CASE(T, mpf_class); \
			EQ_CASE(T, mpq_class); \
		default: \
			throw(std::string("'==' ") + l.type_name() + ", " + r.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. "); \
		} \
		break

		switch(l.type_id()){
			EQ_SWITCH_CASE(mpz_class);
			EQ_SWITCH_CASE(mpf_class);
			EQ_SWITCH_CASE(mpq_class);

			default:
				throw(std::string("'==' ") + l.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. ");
				break;
		}
		return false;
	}

	bool neq_value(const value_holder &l, const value_holder &r){
#define NEQ_CASE(L, R) \
	case value_type_seq_index<R>::value: \
		return *static_cast<L*>(l.ptr()) != *static_cast<R*>(r.ptr());

#define NEQ_SWITCH_CASE(T) \
	case value_type_seq_index<T>::value: \
		switch(r.type_id()){ \
			NEQ_CASE(T, mpz_class); \
			NEQ_CASE(T, mpf_class); \
			NEQ_CASE(T, mpq_class); \
		default: \
			throw(std::string("'!=' ") + l.type_name() + ", " + r.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. "); \
		} \
		break

		switch(l.type_id()){
			NEQ_SWITCH_CASE(mpz_class);
			NEQ_SWITCH_CASE(mpf_class);
			NEQ_SWITCH_CASE(mpq_class);

			default:
				throw(std::string("'!=' ") + l.type_name() + ". î‰ärÇ≈Ç´Ç‹ÇπÇÒ. ");
				break;
		}
		return false;
	}
} }
