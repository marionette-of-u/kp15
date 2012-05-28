#include "parse.h"
#include "parse_operation.h"
#include "built_in_funciton.h"
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>

namespace kp15{ namespace parse{
	std::size_t type_id_counter(){ static std::size_t id = 0; return id++; }

	std::vector<std::string> &type_name_seq_holder(){ static std::vector<std::string> vec; return vec; }
	void set_type_name_seq(const char *str){ type_name_seq_holder().push_back(std::string(str)); }
	const std::string &get_type_name(std::size_t i){ return type_name_seq_holder()[i]; }

	void null_deleter(void*){}

	value_seq::value_seq() : base_type(){}
	value_seq::value_seq(const value_seq &other){
		resize(other.size());
		for(std::size_t i = 0; i < size(); ++i){
			(*this)[i] = other[i].clone();
		}
	}

	value_holder &value_seq::operator [](std::size_t i){ return base_type::operator [](i); }
	const value_holder &value_seq::operator [](std::size_t i) const{ return base_type::operator [](i); }

	//型の名前
	const char **value_str_seq(){
		static const char *strs[] = {
			"integer",
			"float",
			"rational",
			"sequence",
			"vector",
			"vector^T",
			"matrix",
			"identifier",
			"void"
		};
		return strs;
	}

	//予約語, 定数
	const char **reserved_word(){
		static const char *strs[] = {
			"T",
			"PI", "pi",
			"integer",
			"float",
			"rational",
			"erase",
			0
		};
		return strs;
	}

	int integer_to_other_type = value_type_seq_index<mpz_class>::value;

	value_holder::value_holder() : ptr_(0), type_id_(get_type_id<void>()), deleter(null_deleter){}
	value_holder::value_holder(const value_holder &other) : ptr_(other.ptr_), type_id_(other.type_id_), deleter(other.deleter){
		const_cast<value_holder&>(other).clear();
	}

#define CLONE_FN(TYPE) \
	value_holder clone_ ## TYPE(const value_holder &f){ \
		value_holder result; \
		result.assign(new TYPE(*static_cast<TYPE*>(f.ptr()))); \
		return result; \
	}
	CLONE_FN(mpz_class);
	CLONE_FN(mpf_class);
	CLONE_FN(mpq_class);
	CLONE_FN(value_seq);
	CLONE_FN(vector_container);
	CLONE_FN(c_vector_container);
	CLONE_FN(matrix_container);
	CLONE_FN(identifier_container);
#undef CLONE_FN

	value_holder::~value_holder(){ deleter(ptr_); }

	value_holder &value_holder::operator =(const value_holder &other){
		deleter(ptr_);
		ptr_ = other.ptr_;
		type_id_ = other.type_id_;
		deleter = other.deleter;
		const_cast<value_holder&>(other).clear();
		return *this;
	}

	std::string value_holder::get_str() const{
		return partial_get_str(type_id_, ptr_);
	}

	void *value_holder::ptr() const{ return ptr_; }
	std::size_t value_holder::type_id() const{ return type_id_; }
	const std::string &value_holder::type_name() const{ return get_type_name(type_id_); }
	void value_holder::clear(){ ptr_ = 0; deleter = null_deleter; }
#define CLONE_PATTERN(TYPE) \
	case value_type_seq_index<TYPE>::value: \
		return clone_ ## TYPE(*this);

	value_holder value_holder::clone() const{
		switch(type_id()){
			CLONE_PATTERN(mpz_class);
			CLONE_PATTERN(mpf_class);
			CLONE_PATTERN(mpq_class);
			CLONE_PATTERN(value_seq);
			CLONE_PATTERN(vector_container);
			CLONE_PATTERN(c_vector_container);
			CLONE_PATTERN(matrix_container);
			CLONE_PATTERN(identifier_container);
			
			default:
				throw(std::string("'") + type_name() + "' クローンを生成できませんでした. ");
				break;
		}
		return value_holder();
	}

	//get_type_id関数を初期化
	init_type_id_<typename boost::mpl::begin<value_type_seq>::type> init_type_id;

	vector_container::vector_container(const vector_container &other){
		value.assign(new value_seq);
		value_seq *a = static_cast<value_seq*>(value.ptr());
		const value_seq *other_a = static_cast<value_seq*>(other.value.ptr());
		a->resize(other_a->size());
		for(std::size_t i = 0; i < other_a->size(); ++i){
			(*a)[i] = (*other_a)[i].clone();
		}
	}

	c_vector_container::c_vector_container(const c_vector_container &other){
		value.assign(new value_seq);
		value_seq *a = static_cast<value_seq*>(value.ptr());
		const value_seq *other_a = static_cast<value_seq*>(other.value.ptr());
		a->resize(other_a->size());
		for(std::size_t i = 0; i < other_a->size(); ++i){
			(*a)[i] = (*other_a)[i].clone();
		}
	}

	matrix_container::matrix_container(const matrix_container &other){
		value.assign(new value_seq);
		value_seq *a = static_cast<value_seq*>(value.ptr()), *b;
		const value_seq *other_a = static_cast<value_seq*>(other.value.ptr()), *other_b;
		a->resize(other_a->size());
		for(std::size_t i = 0; i < other_a->size(); ++i){
			(*a)[i].assign(b = new value_seq);
			other_b = static_cast<value_seq*>((*other_a)[i].ptr());
			b->resize(other_b->size());
			for(std::size_t j = 0; j < other_b->size(); ++j){ (*b)[j] = (*other_b)[j].clone(); }
		}
	}

	identifier_container::identifier_container(const identifier_container &other) : str(other.str.begin(), other.str.end()){}
	identifier_container::identifier_container(const char *first_, const char *last_) : str(first_, last_){}

	std::string identifier_container_get_str(identifier_container *p){
		return p->str;
	}

	std::string partial_get_str(std::size_t type_id_, void *ptr_){
		switch(type_id_){
			case value_type_seq_index<mpz_class>::value:
				return static_cast<mpz_class*>(ptr_)->get_str();

			case value_type_seq_index<mpf_class>::value:
				return static_cast<mpf_class*>(ptr_)->get_str();

			case value_type_seq_index<mpq_class>::value:
				static_cast<mpq_class*>(ptr_)->canonicalize();
				return static_cast<mpq_class*>(ptr_)->get_str();

			case value_type_seq_index<value_seq>::value:
				{
					std::string str;
					value_seq &seq(*static_cast<value_seq*>(ptr_));
					str += partial_get_str(seq[0].type_id(), seq[0].ptr());
					for(std::size_t i = 1; i < seq.size(); ++i){
						str += ", ";
						str += partial_get_str(seq[i].type_id(), seq[i].ptr());
					}
					return str;
				}

			case value_type_seq_index<vector_container>::value:
				{
					std::string str("[");
					vector_container *ptr = static_cast<vector_container*>(ptr_);
					value_holder &value(ptr->value);
					str += partial_get_str(value.type_id(), value.ptr());
					str += static_cast<char>(']');
					return str;
				}

			case value_type_seq_index<c_vector_container>::value:
				{
					std::string str("<");
					vector_container *ptr = static_cast<vector_container*>(ptr_);
					value_holder &value(ptr->value);
					str += partial_get_str(value.type_id(), value.ptr());
					str += static_cast<char>('>');
					return str;
				}

			case value_type_seq_index<matrix_container>::value:
				{
					std::string str("{");
					value_holder &value(static_cast<vector_container*>(ptr_)->value);
					value_seq &vector_seq(*static_cast<value_seq*>(value.ptr()));
					str += static_cast<char>('[');
					str += partial_get_str(vector_seq[0].type_id(), vector_seq[0].ptr());
					str += static_cast<char>(']');
					for(std::size_t i = 1; i < vector_seq.size(); ++i){
						str += ", ";
						str += static_cast<char>('[');
						str += partial_get_str(vector_seq[i].type_id(), vector_seq[i].ptr());
						str += static_cast<char>(']');
					}
					str += static_cast<char>('}');
					return str;
				}

			case value_type_seq_index<identifier_container>::value:
				return identifier_container_get_str(static_cast<identifier_container*>(ptr_));

			case value_type_seq_index<void_container>::value:
				return "";

			default:
				throw(std::string("表示できない型です. "));
		}
	}

	//変数map
	typedef boost::unordered_map<std::string, value_holder> variable_map_type;
	variable_map_type variable_map;

	//抽象構文木
	class node{
	public:
		virtual ~node(){}
		virtual void put() = 0;
		virtual value_holder calc(){ return value_holder(); }
	};

	class expr : public node{};

	//ユーザー定義関数
	class expr;
	struct user_fn_data{
		user_fn_data() : expr_(0){}
		user_fn_data(const user_fn_data &other) : arg_index_to_str(other.arg_index_to_str), args(other.args){
			expr_ = other.expr_;
			const_cast<user_fn_data&>(other).expr_ = 0;
		}

		~user_fn_data(){ delete expr_; }
		std::vector<std::string> arg_index_to_str;
		typedef boost::unordered_map<std::string, value_holder> args_type;
		args_type args;
		expr *expr_;
	};

	typedef boost::unordered_map<std::string, user_fn_data> user_fn_data_map_type;
	user_fn_data_map_type user_fn_data_map;
	typedef std::vector<user_fn_data*> user_fn_stack_type;
	user_fn_stack_type user_fn_stack;
	typedef boost::unordered_set<std::string> invoked_fn_name_type;
	invoked_fn_name_type invoked_fn_name;

	//重複チェック, 予約語と定数. 
	void check_reserved_word(const std::string &str){
		for(std::size_t i = 0; reserved_word()[i]; ++i){
			if(str == reserved_word()[i]){
				throw(std::string("'") + str + "' 予約されてるキーワードです. ");
			}
		}
	}

	//重複チェック, 関数. 
	void check_reserved_fn(const std::string &str){
		built_in_function::fn_map_type::const_iterator iter = built_in_function::fn_map.find(str);
		if(iter != built_in_function::fn_map.end()){
			throw(std::string("'") + str + "' 関数として使われています. ");
		}
	}

	//重複チェック, 変数
	void check_reserved_var(const std::string &str){
		variable_map_type::const_iterator iter = variable_map.find(str);
		if(iter != variable_map.end()){
			throw(std::string("'") + str + "' 変数として使われています. ");
		}
	}

	//重複チェック, ユーザー定義関数
	void check_reserved_ufn(const std::string &str){
		user_fn_data_map_type::const_iterator iter = user_fn_data_map.find(str);
		if(iter != user_fn_data_map.end()){
			throw(std::string("'") + str + "' ユーザー定義関数として使われています. ");
		}
	}

	class term : public node{};

	class factor : public node{
	public:
		factor(){}
		factor(const char *first_, const char *last_) : str(first_, last_){}
		std::string str;
		const char *first(){ return str.c_str(); }
		const char *last(){ return str.c_str() + str.size(); }
	};

	class integer : public factor{
	public:
		integer(const char *first_, const char *last_) : factor(first_, last_){}
		virtual void put(){
			std::cout << std::string(first(), last());
		}

		virtual value_holder calc(){
			value_holder r;
			switch(integer_to_other_type){
				case value_type_seq_index<mpz_class>::value:
					r.assign(new mpz_class(std::string(first(), last()).c_str()));
					break;

				case value_type_seq_index<mpf_class>::value:
					r.assign(new mpf_class(std::string(first(), last()).c_str()));
					break;

				case value_type_seq_index<mpq_class>::value:
					r.assign(new mpq_class(std::string(first(), last()).c_str()));
					break;
			}
			return r;
		}
	};

	class real : public factor{
	public:
		real(const char *first_, const char *last_) : factor(first_, last_){}
		virtual void put(){
			std::cout << std::string(first(), last());
		}

		virtual value_holder calc(){
			mpf_class *a = new mpf_class(std::string(first(), last()).c_str());
			value_holder r;
			r.assign(a);
			return r;
		}
	};

	class arg : public expr{
	public:
		arg *next_arg, *end_arg;
		expr *expr_;
		arg() : expr_(0), next_arg(0){}
		arg(arg *next_arg_, expr *expr_a) : next_arg(next_arg_), end_arg(0), expr_(expr_a){}
		virtual ~arg(){
			delete expr_;
			delete next_arg;
		}

		std::size_t chain_num(std::size_t num = 1) const{
			if(next_arg){
				return next_arg->chain_num(num + 1);
			}else{
				return num;
			}
		}

		virtual void put(){
			expr_->put();
			if(next_arg){
				std::cout << ", ";
				next_arg->put();
			}
		}

		virtual value_holder calc(){
			value_seq *v = new value_seq;
			v->push_back(expr_->calc());
			if(next_arg){ next_arg->calc_chain(v); }
			value_holder r;
			r.assign(v);
			return r;
		}

	private:
		void calc_chain(value_seq *v){
			v->push_back(expr_->calc());
			if(next_arg){ next_arg->calc_chain(v); }
		}
	};

	template<class T> class basic_vector : public factor{
	public:
		basic_vector() : arg_(0){}
		basic_vector(arg *arg_a) : arg_(arg_a){}
		virtual ~basic_vector(){ delete arg_; }

		virtual value_holder calc(){
			T *v = new T;
			v->value = arg_->calc();
			value_holder r;
			r.assign(v);
			return r;
		}

		arg *arg_;
	};

	class vector : public basic_vector<vector_container>{
	public:
		vector() : basic_vector(){}
		vector(arg *arg_a) : basic_vector(arg_a){}
		virtual void put(){
			std::cout << "[";
			arg_->put();
			std::cout << "]";
		}
	};

	class c_vector : public basic_vector<c_vector_container>{
	public:
		c_vector() : basic_vector(){}
		c_vector(arg *arg_a) : basic_vector(arg_a){}
		virtual void put(){
			std::cout << "<";
			arg_->put();
			std::cout << ">";
		}
	};

	class matrix_terms : public factor{
	public:
		matrix_terms() : vec(0), next_matrix(0){}
		matrix_terms(matrix_terms *next_matrix_, arg *vec_) : next_matrix(next_matrix_), end_matrix(0), vec(vec_){}
		virtual ~matrix_terms(){
			delete vec;
			delete next_matrix;
		}

		std::pair<std::size_t, bool> check_terms_num() const{
			std::pair<std::size_t, bool> r;
			if(next_matrix){
				r = next_matrix->check_terms_num();
				if(!r.second){ return r; }
				r.second = r.first == vec->chain_num();
				return r;
			}else{
				r.first = vec->chain_num();
				r.second = true;
				return r;
			}
		}

		virtual void put(){
			if(end_matrix){ std::cout << "{"; }
			std::cout << "[";
			vec->put();
			std::cout << "]";
			if(next_matrix){
				std::cout << ", ";
				next_matrix->put();
			}else{
				std::cout << "}";
			}
		}

		virtual value_holder calc(){
			std::pair<std::size_t, bool> check_result = check_terms_num();
			if(!check_result.second){ throw(std::string("行の要素数が一致しません. ")); }
			value_seq *v = new value_seq;
			v->push_back(value_holder());
			v->back() = vec->calc();
			if(next_matrix){ next_matrix->calc_chain(v); }
			value_holder q, r;
			matrix_container *m = new matrix_container;
			m->value.assign(v);
			r.assign(m);
			return r;
		}

		matrix_terms *next_matrix, *end_matrix;
		arg *vec;

	private:
		void calc_chain(value_seq *v){
			v->push_back(value_holder());
			v->back() = vec->calc();
			if(next_matrix){ next_matrix->calc_chain(v); }
		}
	};

	class identifier : public factor{
	public:
		identifier(const char *first_, const char *last_) : factor(first_, last_){}
		virtual void put(){
			std::cout << std::string(first(), last());
		}

		virtual value_holder calc(){
			std::string id = std::string(first(), last());
			value_holder r;
			if(translate_switch){
				//定数
				if(id == "pi" || id == "PI"){
					if(mpf_get_default_prec() != current_mp_bitcnt_pi){ calc_pi(mpf_get_default_prec()); }
					r.assign(new mpf_class(mpf_pi));
					return r;
				}
				//関数内の仮引数
				if(scan_user_fn_stack){
					user_fn_data::args_type::const_iterator iter = user_fn_stack.back()->args.find(id);
					if(iter != user_fn_stack.back()->args.end()){
						value_holder r;
						r = iter->second.clone();
						return r;
					}
				}
				//変数
				{
					variable_map_type::const_iterator iter = variable_map.find(id);
					if(iter != variable_map.end()){
						r = iter->second.clone();
						return r;
					}
				}
			}
			//普通にidentifier_containerを返す
			identifier_container *v = new identifier_container(first(), last());
			r.assign(v);
			return r;
		}

		//idを他の何かへ変換するかどうかのswitch
		static bool translate_switch;
		//user_fn_stackをスキャンするかどうかのswitch
		static bool scan_user_fn_stack;
	};

	bool identifier::translate_switch = true;
	bool identifier::scan_user_fn_stack = false;

	template<class Ptr>
	class operator_struct{
	public:
		operator_struct() : lhs(0), rhs(0){}
		operator_struct(Ptr x, Ptr y) : lhs(x), rhs(y){}
		~operator_struct(){ delete lhs; delete rhs; }
		Ptr lhs, rhs;
	};

	class assign_expr : public expr{
	public:
		assign_expr() : identifier_(0), expr_(0){}
		assign_expr(identifier *x, expr *y) : identifier_(x), expr_(y){}
		virtual ~assign_expr(){
			delete expr_;
			delete identifier_;
		}

		virtual void put(){
			std::cout << "(";
			identifier_->put();
			std::cout << " = ";
			expr_->put();
			std::cout << ")";
		}

		virtual value_holder calc(){
			std::string str(identifier_->first(), identifier_->last());
			check_reserved_word(str);
			check_reserved_fn(str);
			check_reserved_var(str);
			check_reserved_ufn(str);
			value_holder result;
			result = expr_->calc();
			variable_map[str] = result.clone();
			return result;
		}

	private:
		identifier *identifier_;
		expr *expr_;
	};

	class user_function : public expr{
	public:
		user_function(identifier *x, arg *y, expr *z) : identifier_(x), arg_(y), expr_(z){}
		virtual ~user_function(){
			delete identifier_;
			delete arg_;
			//expr_はuser_fn_dataで取り扱うのでdeleteしない
		}

		virtual void put(){
			identifier_->put();
		}

		virtual value_holder calc(){
			check_reserved_word(identifier_->str);
			check_reserved_fn(identifier_->str);
			check_reserved_var(identifier_->str);
			check_reserved_ufn(identifier_->str);

			identifier::translate_switch = false;
			value_holder seq_holder;
			seq_holder = arg_->calc();
			value_seq *seq = static_cast<value_seq*>(seq_holder.ptr());

			for(std::size_t i = 0; i < seq->size(); ++i){
				if((*seq)[i].type_id() != value_type_seq_index<identifier_container>::value){
					identifier::translate_switch = true;
					throw(std::string("'") + identifier_->str + "' 仮引数中に識別子以外のトークンを検出しました. ");
				}
			}

			user_fn_data &data(user_fn_data_map[identifier_->str]);
			for(std::size_t i = 0; i < seq->size(); ++i){
				identifier_container *ptr = static_cast<identifier_container*>((*seq)[i].ptr());
				data.args[ptr->str];
				data.arg_index_to_str.push_back(ptr->str);
			}
			data.expr_ = expr_;

			identifier::translate_switch = true;

			value_holder r;
			r.assign(new void_container);
			return r;
		}

	private:
		identifier *identifier_;
		arg *arg_;
		expr *expr_;
	};

	class command_expr : public expr{
	public:
		command_expr() : identifier_x(0), identifier_y(0){}
		command_expr(identifier *x, identifier *y) : identifier_x(x), identifier_y(y){}
		virtual ~command_expr(){
			delete identifier_x;
			delete identifier_y;
		}

		virtual void put(){
			std::cout << "void(";
			identifier_x->put();
			std::cout << " " ;
			identifier_y->put();
			std::cout << ")";
		}

		virtual value_holder calc(){
			std::string str_x(identifier_x->first(), identifier_x->last()), str_y(identifier_y->first(), identifier_y->last());
			if(str_x == "erase"){
				variable_map_type::iterator var_iter = variable_map.find(str_y);
				user_fn_data_map_type::iterator ufn_iter = user_fn_data_map.find(str_y);
				if(var_iter != variable_map.end()){
					variable_map.erase(var_iter);
				}else if(ufn_iter != user_fn_data_map.end()){
					user_fn_data_map.erase(ufn_iter);
				}else{
					throw(std::string("'erase ") + str_y + "' トークンが見つかりません. ");
				}
			}
			value_holder r;
			r.assign(new void_container);
			return r;
		}

	private:
		identifier *identifier_x, *identifier_y;
	};

	class term_expr : public expr{
	public:
		term_expr() : term_(0){}
		term_expr(term *x) : term_(x){}
		virtual ~term_expr(){ delete term_; }
		term *term_;
		virtual void put(){ term_->put(); }

		virtual value_holder calc(){
			return term_->calc();
		}
	};

	class add_expr : public expr, public operator_struct<expr*>{
	public:
		add_expr(expr *x, expr *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << " + "; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc();
			return add_value(l, r);
		}
	};

	class sub_expr : public expr, public operator_struct<expr*>{
	public:
		sub_expr(expr *x, expr *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << " - "; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc();
			return sub_value(l, r);
		}
	};

	class mod_expr : public expr, public operator_struct<expr*>{
	public:
		mod_expr(expr *x, expr *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << " % "; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc();
			return mod_value(l, r);
		}
	};

	class factor_term : public term{
	public:
		factor_term() : factor_(0){}
		factor_term(factor *x) : factor_(x){}
		virtual ~factor_term(){ delete factor_; }
		virtual void put(){ factor_->put(); }
		virtual value_holder calc(){ return factor_->calc(); }
		factor *factor_;
	};

	class mul_term : public term, public operator_struct<term*>{
	public:
		mul_term(term *x, term *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << " * "; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc();
			return mul_value(l, r);
		}
	};

	class div_term : public term, public operator_struct<term*>{
	public:
		div_term(term *x, term *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << " / "; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc();
			return div_value(l, r);
		}
	};

	class exprod_term : public term, public operator_struct<term*>{
	public:
		exprod_term(term *x, term *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << " ** "; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc();
			return exprod_value(l, r);
		}
	};

	class dot_term : public term, public operator_struct<term*>{
	public:
		dot_term(term *x, term *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << " . "; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc();
			return dot_value(l, r);
		}
	};

	class fn_expr : public expr{
	public:
		fn_expr() : expr_x(0), expr_y(0){}
		fn_expr(expr *x, expr *y) : expr_x(x), expr_y(y){}
		virtual ~fn_expr(){
			delete expr_x;
			delete expr_y;
		}

		virtual void put(){
			std::cout << "(";
			expr_y->put();
			std::cout << " -> ";
			expr_x->put();
			std::cout << ")";
		}

		virtual value_holder calc(){
			value_holder x = expr_x->calc();
			value_seq *value_seq_ptr;
			if(x.type_id() != value_type_seq_index<value_seq>::value){
				value_holder n;
				n.assign(value_seq_ptr = new value_seq);
				value_seq_ptr->push_back(x);
				x.clear();
				x = n;
			}else{
				value_seq_ptr = static_cast<value_seq*>(x.ptr());
			}

			value_holder y = expr_y->calc();
			if(y.type_id() == value_type_seq_index<identifier_container>::value){
				std::string fn_name(static_cast<identifier_container*>(y.ptr())->str);
				built_in_function::fn_map_type::const_iterator iter;
				user_fn_data_map_type::iterator ufn_iter;
				if(
					(iter = built_in_function::fn_map.find(fn_name)) == built_in_function::fn_map.end() &&
					(ufn_iter = user_fn_data_map.find(fn_name)) == user_fn_data_map.end()
				){ throw(std::string("'") + fn_name + "' 関数が存在しません. "); }
				if(iter != built_in_function::fn_map.end()){
					//組み込み関数
					const built_in_function::fn_struct &bf_struct(iter->second);
					std::size_t arg_num;
					if(bf_struct.arg_num > 0 && bf_struct.arg_num != (arg_num = value_seq_ptr->size())){
						throw(
							std::string("'") + fn_name + "' 引数が一致しません. " +
							boost::lexical_cast<std::string>(bf_struct.arg_num) + "引数に対して, " +
							boost::lexical_cast<std::string>(arg_num) + "引数が指定されました. "
						);
					}
					return bf_struct.fn(*value_seq_ptr);
				}else if(ufn_iter != user_fn_data_map.end()){
					//ユーザー定義関数
					if(invoked_fn_name.find(fn_name) != invoked_fn_name.end()){
						throw(std::string("'") + fn_name + "' 再帰を検出しました. ");
					}
					if(user_fn_stack.size() == 0){ identifier::scan_user_fn_stack = true;}
					invoked_fn_name.insert(fn_name);
					user_fn_stack.push_back(&(ufn_iter->second));
					std::size_t arg_num;
					user_fn_data *ufd_ptr = user_fn_stack.back();
					if(ufd_ptr->args.size() != (arg_num = value_seq_ptr->size())){
						throw(
							std::string("'") + fn_name + "' 引数が一致しません. " +
							boost::lexical_cast<std::string>(ufd_ptr->args.size()) + "引数に対して, " +
							boost::lexical_cast<std::string>(arg_num) + "引数が指定されました. "
						);
					}
					for(std::size_t i = 0; i < ufd_ptr->args.size(); ++i){
						ufd_ptr->args[ufd_ptr->arg_index_to_str[i]] = (*value_seq_ptr)[i].clone();
					}
					value_holder r;

#define TRY_CATCH_FINALLY \
	user_fn_stack.pop_back(); \
	invoked_fn_name.erase(fn_name); \
	if(user_fn_stack.size() == 0){ identifier::scan_user_fn_stack = false;}
					try{
						r = ufn_iter->second.expr_->calc();
					}catch(std::string err){
						TRY_CATCH_FINALLY;
						throw(err);
					}
					TRY_CATCH_FINALLY;
#undef TRY_CATCH_FINALLY
					return r;
				}
			}else{
				value_seq_ptr->push_back(y);
			}

			return x;
		}

		expr *expr_x;
		expr *expr_y;
	};

	class pow_factor : public factor, public operator_struct<factor*>{
	public:
		pow_factor(factor *x, factor *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << "^"; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){ return pow_value(lhs->calc(), rhs->calc()); }
	};

	class fact_factor : public factor{
	public:
		fact_factor(factor *x) : factor_(x){}
		virtual void put(){ factor_->put(); std::cout << "!"; }
		virtual value_holder calc(){ return fact_value(factor_->calc()); }

	private:
		factor *factor_;
	};

	class rational_factor : public factor, public operator_struct<factor*>{
	public:
		rational_factor(factor *x, factor *y) : operator_struct(x, y){}
		virtual void put(){ std::cout << "("; lhs->put(); std::cout << "//"; rhs->put(); std::cout << ")"; }
		virtual value_holder calc(){
			value_holder l = lhs->calc(), r = rhs->calc(), result;
			return rational_value(l, r);
		}
	};

	class negate_factor : public factor{
	public:
		negate_factor() : factor_(0){}
		negate_factor(factor *x) : factor_(x){}
		virtual ~negate_factor(){ delete factor_; }
		virtual void put(){ std::cout << "-("; factor_->put(); std::cout << ")"; }
		factor *factor_;
		virtual value_holder calc(){ return nagete_value(factor_->calc()); }
	};

	class cast_factor : public factor{
	public:
		cast_factor() : identifier_(0), expr_(0){}
		cast_factor(identifier *x, expr *y) : identifier_(x), expr_(y){}
		virtual ~cast_factor(){
			delete identifier_;
			delete expr_;
		}

		virtual void put(){ identifier_->put(); std::cout << "("; expr_->put(); std::cout << ")"; }
		identifier *identifier_;
		expr *expr_;
		virtual value_holder calc(){ return cast_value(expr_->calc(), std::string(identifier_->first(), identifier_->last())); }
	};

	class integer_cast_factor : public factor{
	public:
		integer_cast_factor() : identifier_(0), expr_(0){}
		integer_cast_factor(identifier *x, expr *y) : identifier_(x), expr_(y){}
		virtual ~integer_cast_factor(){
			delete identifier_;
			delete expr_;
		}

		virtual void put(){ identifier_->put(); std::cout << "("; expr_->put(); std::cout << ")"; }
		identifier *identifier_;
		expr *expr_;
		virtual value_holder calc(){
			std::string str(identifier_->first(), identifier_->last());
			if(str == "integer"){
				integer_to_other_type = value_type_seq_index<mpz_class>::value;
			}else if(str == "float"){
				integer_to_other_type = value_type_seq_index<mpf_class>::value;
			}else if(str == "rational"){
				integer_to_other_type = value_type_seq_index<mpq_class>::value;
			}
			return expr_->calc();
		}
	};

	class precision_cast_factor : public factor{
	public:
		precision_cast_factor() : identifier_(0), expr_(0){}
		precision_cast_factor(identifier *x, expr *y) : identifier_(x), expr_(y){}
		virtual ~precision_cast_factor(){
			delete identifier_;
			delete expr_;
		}

		virtual void put(){ identifier_->put(); std::cout << "("; expr_->put(); std::cout << ")"; }
		identifier *identifier_;
		expr *expr_;
		virtual value_holder calc(){
			std::string prec_str(identifier_->first() + 1, identifier_->last());
			mpf_set_default_prec(std::strtoul(prec_str.c_str(), 0, 10));
			return precision_cast_value(expr_->calc());
		}
	};

	class factor_expr : public factor{
	public:
		factor_expr() : expr_(0){}
		factor_expr(expr *x) : expr_(x){}
		virtual ~factor_expr(){ delete expr_; }
		virtual void put(){ expr_->put(); }
		expr *expr_;
		virtual value_holder calc(){
			return expr_->calc();
		}
	};

	//セマンティックアクション
	struct semantic_action{
		typedef calc::Stack<calc::Parser<node*, semantic_action>::stack_frame, 0> stack_type;

		void syntax_error(const stack_type &stack){
			for(std::size_t i = 0; i < stack.stack_.size(); ++i){
				delete stack.stack_[i].value;
			}
			throw(std::string("構文解析エラー. "));
		}

		void stack_overflow(){ throw(std::string("構文解析に用いるスタックがオーバーフローしました. ")); }

		template<class T> void downcast(T* &x, node *y){ x = static_cast<T*>(y); }
		template<class T> void upcast(node* &x, T *y){ x = y; }

		expr *make_top_level_expr(expr *x){ return x; }

		expr *make_assign_expr(expr *x){ return x; }
		expr *make_assign(identifier *x, expr *y){ return new assign_expr(x, y); }
		expr *make_define_function(identifier *x, arg *y, expr *z){ return new user_function(x, y, z); }

		expr *make_command(identifier *x, identifier *y){ return new command_expr(x, y); }

		expr *make_lr_fn_expr(expr *x){ return x; }
		expr *make_lr_function(expr *x, expr *y){ return new fn_expr(y, x); }

		arg *make_r_arg(expr *x){ return new arg(reinterpret_cast<arg*>(0), x); }
		arg *make_r_args(arg *x, expr *y){
			arg *p(new arg(reinterpret_cast<arg*>(0), y));
			if(x->end_arg){
				x->end_arg->next_arg = p;
				x->end_arg = p;
			}else{
				x->end_arg = x->next_arg = p;
			}
			return x;
		}

		arg *make_l_arg(expr *x){ return new arg(reinterpret_cast<arg*>(0), x); }
		arg *make_l_args(arg *x, expr *y){ return make_r_args(x, y); }

		expr *make_expr(term *x){ return new term_expr(x); }
		expr *make_add(expr *x, term *y){ return new add_expr(x, make_expr(y)); }
		expr *make_sub(expr *x, term *y){ return new sub_expr(x, make_expr(y)); }
		expr *make_mod(expr *x, term *y){ return new mod_expr(x, make_expr(y)); }

		term *make_term(factor *x){ return new factor_term(x); }
		term *make_mul(term *x, factor *y){ return new mul_term(x, make_term(y)); }
		term *make_div(term *x, factor *y){ return new div_term(x, make_term(y)); }
		term *make_exprod(term *x, factor *y){ return new exprod_term(x, make_term(y)); }
		term *make_dot(term *x, factor *y){ return new dot_term(x, make_term(y)); }

		arg *make_arg(expr *x){ return new arg(reinterpret_cast<arg*>(0), x); }
		arg *make_args(arg *x, expr *y){
			arg *p(new arg(reinterpret_cast<arg*>(0), y));
			if(x->end_arg){
				x->end_arg->next_arg = p;
				x->end_arg = p;
			}else{
				x->end_arg = x->next_arg = p;
			}
			return x;
		}

		factor *make_middle_factor(factor *x){ return x; }
		factor *make_pow(factor *x, factor *y){ return new pow_factor(x, y); }
		factor *make_pow_negate(factor *x, factor *y){ return new pow_factor(x, make_negate(y)); }

		factor *make_fact_factor(factor *x){ return x; }
		factor *make_fact(factor *x){ return new fact_factor(x); }

		factor *make_rational_factor(factor *x){ return x; }
		factor *make_rational(factor *x, factor *y){ return new rational_factor(x, y); }
		factor *make_rational_negate(factor *x, factor *y){ return make_negate(new rational_factor(x, y)); }

		factor *make_unary_operation_factor(factor *x){ return x; }
		factor *make_negate(factor *x){ return new negate_factor(x); }

		factor *make_integer(integer *x){ return x; }
		factor *make_real(real *x){ return x; }
		factor *make_vector(arg *x){ return new vector(x); }
		factor *make_c_vector(arg *x){ return new c_vector(x); }
		factor *make_matrix(matrix_terms *x){ return x; }
		factor *make_identifier(identifier *x){ return x; }
		factor *make_function(identifier *x, arg *y){ return make_factor_expr(new fn_expr(y, make_expr(make_term(x)))); }
		factor *make_cast(identifier *x, expr *y){ return new cast_factor(x, y); }
		factor *make_integer_cast(identifier *x, expr *y){ return new integer_cast_factor(x, y); }
		factor *make_precision_cast(identifier *x, expr *y){ return new precision_cast_factor(x, y); }
		factor *make_factor_expr(expr *x){ return new factor_expr(x); }

		matrix_terms *make_matrix_term(arg *x){ return new matrix_terms(reinterpret_cast<matrix_terms*>(0), x); }
		matrix_terms *make_matrix_terms(matrix_terms *x, arg *y){
			matrix_terms *p(new matrix_terms(reinterpret_cast<matrix_terms*>(0), y));
			if(x->end_matrix){
				x->end_matrix->next_matrix = p;
				x->end_matrix = p;
			}else{
				x->end_matrix = x->next_matrix = p;
			}
			return x;
		}
	};
} }
