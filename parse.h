#pragma once

#include <boost/type_traits.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/next_prior.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/find.hpp>

namespace kp15{ namespace parse{
	std::size_t type_id_counter();

	//型IDを取得
	template<class T> std::size_t get_type_id(){ static std::size_t a = type_id_counter(); return a; }

	//型の名前を取得
	std::vector<std::string> &type_name_seq_holder();
	void set_type_name_seq(const char *str);
	const std::string &get_type_name(std::size_t i);

	//tempalate削除子
	template<class T>
	void template_deleter(void *ptr){ delete reinterpret_cast<T*>(ptr); }

	//何もしない削除子
	void null_deleter(void*);

	class value_holder;

	//値の列
	class value_seq : public std::vector<value_holder>{
	private:
		typedef std::vector<value_holder> base_type;

	public:
		value_seq();
		value_seq(const value_seq &other);
		value_holder &operator [](std::size_t);
		const value_holder &operator [](std::size_t) const;
	};

	class vector_container;
	class c_vector_container;
	class matrix_container;
	struct identifier_container;
	struct void_container;
	std::string identifier_container_get_str(identifier_container*);

	//value_holderに格納される型のシーケンス
	typedef boost::mpl::vector<
		mpz_class,
		mpf_class,
		mpq_class,
		value_seq,
		vector_container,
		c_vector_container,
		matrix_container,
		identifier_container,
		void_container
	> value_type_seq;

	//value_holderに格納される型の文字列
	const char **value_str_seq();

	template<class Type>
	struct value_type_seq_index{
		enum{ value = boost::mpl::find<value_type_seq, Type>::type::pos::value };
	};

	std::string partial_get_str(std::size_t type_id_, void *ptr_);

	//値を保持する
	class value_holder{
	public:
		value_holder();
		value_holder(const value_holder &other);

		~value_holder();
		value_holder &operator =(const value_holder &other);

		std::string get_str() const;

		template<class T>
		inline void assign(T *ptr_a){
			deleter(ptr_);
			ptr_ = ptr_a;
			type_id_ = get_type_id<T>();
			deleter = template_deleter<T>;
		}

		void *ptr() const;
		std::size_t type_id() const;
		const std::string &type_name() const;
		value_holder clone() const;
		void clear();

	private:
		void *ptr_;
		std::size_t type_id_;
		void (*deleter)(void*);
	};

	//get_type_id関数を初期化するメタ関数
	template<class Iter>
	struct init_type_id_{
		typedef typename boost::mpl::deref<Iter>::type type;
		init_type_id_(){
			apply();
		};

		static void apply(){
			get_type_id<type>();
			set_type_name_seq(value_str_seq()[get_type_id<type>()]);
			init_type_id_<typename boost::mpl::next<Iter>::type>::apply();
		}
	};

	template<>
	struct init_type_id_<boost::mpl::end<value_type_seq>::type>{
		static void apply(){}
	};

	//vectorのコンテナ
	class vector_container{
	public:
		inline vector_container(){}
		vector_container(const vector_container&);
		//value_seq > value
		value_holder value;
	};

	//列vectorのコンテナ
	class c_vector_container{
	public:
		inline c_vector_container(){}
		c_vector_container(const c_vector_container&);
		//value_seq > value
		value_holder value;
	};

	inline std::size_t vector_size(const value_holder &f){
		return static_cast<value_seq*>(static_cast<vector_container*>(f.ptr())->value.ptr())->size();
	}

	inline std::size_t c_vector_size(const value_holder &f){
		return static_cast<value_seq*>(static_cast<c_vector_container*>(f.ptr())->value.ptr())->size();
	}

	inline value_holder &vector_element(const value_holder &f, std::size_t i){
		return (*static_cast<value_seq*>(static_cast<vector_container*>(f.ptr())->value.ptr()))[i];
	}

	inline value_holder &c_vector_element(const value_holder &f, std::size_t i){
		return (*static_cast<value_seq*>(static_cast<c_vector_container*>(f.ptr())->value.ptr()))[i];
	}

	//matrixのコンテナ
	class matrix_container{
	public:
		inline matrix_container(){}
		matrix_container(const matrix_container&);
		//value_seq > value_seq > value
		value_holder value;
	};

	inline std::size_t matrix_colum_size(const value_holder &f){
		return static_cast<value_seq*>(static_cast<matrix_container*>(f.ptr())->value.ptr())->size();
	}

	inline std::size_t matrix_row_size(const value_holder &f){
		return static_cast<value_seq*>((*static_cast<value_seq*>(static_cast<matrix_container*>(f.ptr())->value.ptr()))[0].ptr())->size();
	}

	inline value_holder &matrix_element(const value_holder &f, std::size_t i, std::size_t j){
		return (*static_cast<value_seq*>((*static_cast<value_seq*>(static_cast<matrix_container*>(f.ptr())->value.ptr()))[i].ptr()))[j];
	}

	inline value_seq &matrix_row(const value_holder &f, std::size_t i){
		return *static_cast<value_seq*>((*static_cast<value_seq*>(static_cast<matrix_container*>(f.ptr())->value.ptr()))[i].ptr());
	}

	//識別子のコンテナ
	struct identifier_container{
		inline identifier_container(){}
		identifier_container(const identifier_container&);
		identifier_container(const char *first_, const char *last_);
		std::string str;
	};

	//何もないコンテナ
	struct void_container{};

	//integerを他の型に自動変換するかどうか
	extern int integer_to_other_type;
} }
