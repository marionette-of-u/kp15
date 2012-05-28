#ifndef PARSER_HPP_
#define PARSER_HPP_

#include <cstdlib>
#include <cassert>
#include <vector>

namespace calc {

enum Token {
    token_eof,
    token_add,
    token_built_in_type,
    token_comma,
    token_div,
    token_dot,
    token_eol,
    token_equal,
    token_exclamation,
    token_exprod,
    token_float_precision,
    token_id,
    token_integer,
    token_l_allow,
    token_l_brace,
    token_l_bracket,
    token_l_broket,
    token_l_pare,
    token_mod,
    token_mul,
    token_null,
    token_pow,
    token_r_allow,
    token_r_brace,
    token_r_bracket,
    token_r_broket,
    token_r_pare,
    token_rational,
    token_real,
    token_sub,
};

template < class T, int StackSize >
class Stack {
public:
	Stack(){ gap_ = 0; }
	~Stack(){}
	
	void reset_tmp()
	{
		gap_ = stack_.size();
		tmp_.clear();
	}

	void commit_tmp()
	{
		// may throw
		stack_.reserve( gap_ + tmp_.size() );

		// expect not to throw
stack_.erase( stack_.begin() + gap_, stack_.end() );
		stack_.insert( stack_.end(), tmp_.begin(), tmp_.end() );
	}
	bool push( const T& f )
	{
		if( StackSize != 0 && StackSize <= stack_.size() + tmp_.size() ) {
			return false;
		}
		tmp_.push_back( f );
		return true;
	}

	void pop( size_t n )
	{
		if( tmp_.size() < n ) {
			n -= tmp_.size();
			tmp_.clear();
			gap_ -= n;
		} else {
			tmp_.erase( tmp_.end() - n, tmp_.end() );
		}
	}

	const T& top()
	{
		if( !tmp_.empty() ) {
			return tmp_.back();
		} else {
			return stack_[ gap_ - 1 ];
		}
	}

	const T& get_arg( size_t base, size_t index )
	{
		size_t n = tmp_.size();
		if( base - index <= n ) {
			return tmp_[ n - ( base - index ) ];
		} else {
			return stack_[ gap_ - ( base - n ) + index ];
		}
	}

	void clear()
	{
		stack_.clear();
	}

	std::vector< T > stack_;
	std::vector< T > tmp_;
	size_t gap_;

};

template < class Value, class SemanticAction, int StackSize = 0 >
class Parser {
public:
	typedef Token token_type;
	typedef Value value_type;

public:
	Parser( SemanticAction& sa ) : sa_( sa ) { reset(); }

	void reset()
	{
		error_ = false;
		accepted_ = false;
		clear_stack();
		reset_tmp_stack();
		if( push_stack( &Parser::state_0, &Parser::gotof_0, value_type() ) ) {
			commit_tmp_stack();
		} else {
			sa_.stack_overflow();
			error_ = true;
		}
	}

	bool post( token_type token, const value_type& value )
	{
		assert( !error_ );
		reset_tmp_stack();
		while( (this->*(stack_top()->state) )( token, value ) ); // may throw
		if( !error_ ) {
			commit_tmp_stack();
		}
		return accepted_ || error_;
	}

	bool accept( value_type& v )
	{
		assert( accepted_ );
		if( error_ ) { return false; }
		v = accepted_value_;
		return true;
	}

	bool error() { return error_; }

private:
	typedef Parser< Value, SemanticAction, StackSize > self_type;
	typedef bool ( self_type::*state_type )( token_type, const value_type& );
	typedef bool ( self_type::*gotof_type )( int, const value_type& );

	bool            accepted_;
	bool            error_;
	value_type      accepted_value_;

	SemanticAction& sa_;

public:
	struct stack_frame {
		state_type state;
		gotof_type gotof;
		value_type value;

		stack_frame( state_type s, gotof_type g, const value_type& v )
		    : state( s ), gotof( g ), value( v ) {}
	};

	Stack< stack_frame, StackSize > stack_;

private:
	bool push_stack( state_type s, gotof_type g, const value_type& v )
	{
		bool f = stack_.push( stack_frame( s, g, v ) );
		assert( !error_ );
		if( !f ) {
			error_ = true;
			sa_.stack_overflow();
		}
		return f;
	}

	void pop_stack( size_t n )
	{
		stack_.pop( n );
	}

	const stack_frame* stack_top()
	{
		return &stack_.top();
	}

	const value_type& get_arg( size_t base, size_t index )
	{
		return stack_.get_arg( base, index ).value;
	}

	void clear_stack()
	{
		stack_.clear();
	}

	void reset_tmp_stack()
	{
		stack_.reset_tmp();
	}

	void commit_tmp_stack()
	{
		stack_.commit_tmp();
	}

	bool call_0_make_assign( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		expr* r = sa_.make_assign( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_command( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		identifier* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		expr* r = sa_.make_command( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_define_function( int nonterminal_index, int base, int arg_index0, int arg_index1, int arg_index2 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		arg* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		expr* arg2; sa_.downcast( arg2, get_arg( base, arg_index2 ) );
		expr* r = sa_.make_define_function( arg0, arg1, arg2 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_assign_expr( int nonterminal_index, int base, int arg_index0 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* r = sa_.make_assign_expr( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_args( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		arg* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		arg* r = sa_.make_args( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_arg( int nonterminal_index, int base, int arg_index0 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		arg* r = sa_.make_arg( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_add( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		term* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		expr* r = sa_.make_add( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_mod( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		term* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		expr* r = sa_.make_mod( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_sub( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		term* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		expr* r = sa_.make_sub( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_expr( int nonterminal_index, int base, int arg_index0 )
	{
		term* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* r = sa_.make_expr( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_fact( int nonterminal_index, int base, int arg_index0 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_fact( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_fact_factor( int nonterminal_index, int base, int arg_index0 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_fact_factor( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_integer_cast( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_integer_cast( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_cast( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_cast( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_precision_cast( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_precision_cast( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_identifier( int nonterminal_index, int base, int arg_index0 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_identifier( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_function( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		identifier* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		arg* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_function( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_integer( int nonterminal_index, int base, int arg_index0 )
	{
		integer* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_integer( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_matrix( int nonterminal_index, int base, int arg_index0 )
	{
		matrix_terms* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_matrix( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_vector( int nonterminal_index, int base, int arg_index0 )
	{
		arg* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_vector( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_c_vector( int nonterminal_index, int base, int arg_index0 )
	{
		arg* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_c_vector( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_factor_expr( int nonterminal_index, int base, int arg_index0 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_factor_expr( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_real( int nonterminal_index, int base, int arg_index0 )
	{
		real* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_real( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_lr_fn_expr( int nonterminal_index, int base, int arg_index0 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* r = sa_.make_lr_fn_expr( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_lr_function( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		expr* r = sa_.make_lr_function( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_matrix_term( int nonterminal_index, int base, int arg_index0 )
	{
		arg* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		matrix_terms* r = sa_.make_matrix_term( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_matrix_terms( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		matrix_terms* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		arg* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		matrix_terms* r = sa_.make_matrix_terms( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_middle_factor( int nonterminal_index, int base, int arg_index0 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_middle_factor( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_pow_negate( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_pow_negate( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_pow( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_pow( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_rational_factor( int nonterminal_index, int base, int arg_index0 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_rational_factor( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_rational_negate( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_rational_negate( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_rational( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		factor* r = sa_.make_rational( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_div( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		term* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		term* r = sa_.make_div( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_dot( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		term* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		term* r = sa_.make_dot( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_exprod( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		term* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		term* r = sa_.make_exprod( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_mul( int nonterminal_index, int base, int arg_index0, int arg_index1 )
	{
		term* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* arg1; sa_.downcast( arg1, get_arg( base, arg_index1 ) );
		term* r = sa_.make_mul( arg0, arg1 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_term( int nonterminal_index, int base, int arg_index0 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		term* r = sa_.make_term( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_top_level_expr( int nonterminal_index, int base, int arg_index0 )
	{
		expr* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		expr* r = sa_.make_top_level_expr( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_negate( int nonterminal_index, int base, int arg_index0 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_negate( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool call_0_make_unary_operation_factor( int nonterminal_index, int base, int arg_index0 )
	{
		factor* arg0; sa_.downcast( arg0, get_arg( base, arg_index0 ) );
		factor* r = sa_.make_unary_operation_factor( arg0 );
		value_type v; sa_.upcast( v, r );
		pop_stack( base );
		return (this->*(stack_top()->gotof))( nonterminal_index, v );
	}

	bool gotof_0( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_1, &Parser::gotof_1, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_0( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_1( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_1( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
			// accept
			// run_semantic_action();
			accepted_ = true;
			accepted_value_  = get_arg( 1, 0 );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_2( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_33, &Parser::gotof_33, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 1: return push_stack( &Parser::state_18, &Parser::gotof_18, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_2( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_3( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_33, &Parser::gotof_33, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 1: return push_stack( &Parser::state_34, &Parser::gotof_34, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_3( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_4( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_33, &Parser::gotof_33, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 1: return push_stack( &Parser::state_35, &Parser::gotof_35, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_4( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_5( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_33, &Parser::gotof_33, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 1: return push_stack( &Parser::state_36, &Parser::gotof_36, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_5( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_6( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_33, &Parser::gotof_33, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 1: return push_stack( &Parser::state_37, &Parser::gotof_37, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_6( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_7( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_33, &Parser::gotof_33, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 1: return push_stack( &Parser::state_38, &Parser::gotof_38, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_7( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_8( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_39, &Parser::gotof_39, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_8( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_9( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_74, &Parser::gotof_74, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_9( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_10( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_76, &Parser::gotof_76, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_10( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_11( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_79, &Parser::gotof_79, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_11( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_12( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 10: return push_stack( &Parser::state_81, &Parser::gotof_81, v );
		case 0: return push_stack( &Parser::state_13, &Parser::gotof_13, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_12( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_13( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_13( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_comma:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_top_level_expr( 10, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_14( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 0: return push_stack( &Parser::state_17, &Parser::gotof_17, v );
		case 5: return push_stack( &Parser::state_15, &Parser::gotof_15, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_14( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_16, &Parser::gotof_16, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_15( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_15( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_r_allow:
			// shift
			push_stack( &Parser::state_24, &Parser::gotof_24, value );
			return false;
		case token_eof:
		case token_comma:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_assign_expr( 0, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_16( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_16( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_equal:
			// shift
			push_stack( &Parser::state_14, &Parser::gotof_14, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_22, &Parser::gotof_22, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_2, &Parser::gotof_2, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_identifier( 4, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_17( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_17( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_comma:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_assign( 0, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_18( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_18( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
			// shift
			push_stack( &Parser::state_8, &Parser::gotof_8, value );
			return false;
		case token_r_pare:
			// shift
			push_stack( &Parser::state_19, &Parser::gotof_19, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_19( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_19( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_equal:
			// shift
			push_stack( &Parser::state_20, &Parser::gotof_20, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_function( 4, 4, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_20( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 5: return push_stack( &Parser::state_21, &Parser::gotof_21, v );
		case 2: return push_stack( &Parser::state_23, &Parser::gotof_23, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_20( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_21( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_21( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_r_allow:
			// shift
			push_stack( &Parser::state_24, &Parser::gotof_24, value );
			return false;
		case token_eof:
		case token_comma:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_define_function( 0, 6, 0, 2, 5 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_22( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_22( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_comma:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_command( 0, 2, 0, 1 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_23( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_23( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_add:
			// shift
			push_stack( &Parser::state_27, &Parser::gotof_27, value );
			return false;
		case token_mod:
			// shift
			push_stack( &Parser::state_31, &Parser::gotof_31, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_29, &Parser::gotof_29, value );
			return false;
		case token_eof:
		case token_comma:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_lr_fn_expr( 5, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_24( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 2: return push_stack( &Parser::state_25, &Parser::gotof_25, v );
		case 9: return push_stack( &Parser::state_26, &Parser::gotof_26, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_24( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_25( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_25( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_add:
			// shift
			push_stack( &Parser::state_27, &Parser::gotof_27, value );
			return false;
		case token_mod:
			// shift
			push_stack( &Parser::state_31, &Parser::gotof_31, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_29, &Parser::gotof_29, value );
			return false;
		case token_eof:
		case token_comma:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_lr_function( 5, 3, 2, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_26( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_26( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_div:
			// shift
			push_stack( &Parser::state_45, &Parser::gotof_45, value );
			return false;
		case token_dot:
			// shift
			push_stack( &Parser::state_47, &Parser::gotof_47, value );
			return false;
		case token_exprod:
			// shift
			push_stack( &Parser::state_43, &Parser::gotof_43, value );
			return false;
		case token_mul:
			// shift
			push_stack( &Parser::state_41, &Parser::gotof_41, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_mod:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_expr( 2, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_27( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 9: return push_stack( &Parser::state_28, &Parser::gotof_28, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_27( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_28( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_28( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_div:
			// shift
			push_stack( &Parser::state_45, &Parser::gotof_45, value );
			return false;
		case token_dot:
			// shift
			push_stack( &Parser::state_47, &Parser::gotof_47, value );
			return false;
		case token_exprod:
			// shift
			push_stack( &Parser::state_43, &Parser::gotof_43, value );
			return false;
		case token_mul:
			// shift
			push_stack( &Parser::state_41, &Parser::gotof_41, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_mod:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_add( 2, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_29( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 9: return push_stack( &Parser::state_30, &Parser::gotof_30, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_29( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_30( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_30( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_div:
			// shift
			push_stack( &Parser::state_45, &Parser::gotof_45, value );
			return false;
		case token_dot:
			// shift
			push_stack( &Parser::state_47, &Parser::gotof_47, value );
			return false;
		case token_exprod:
			// shift
			push_stack( &Parser::state_43, &Parser::gotof_43, value );
			return false;
		case token_mul:
			// shift
			push_stack( &Parser::state_41, &Parser::gotof_41, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_mod:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_sub( 2, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_31( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 9: return push_stack( &Parser::state_32, &Parser::gotof_32, v );
		case 11: return push_stack( &Parser::state_40, &Parser::gotof_40, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_31( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_32( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_32( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_div:
			// shift
			push_stack( &Parser::state_45, &Parser::gotof_45, value );
			return false;
		case token_dot:
			// shift
			push_stack( &Parser::state_47, &Parser::gotof_47, value );
			return false;
		case token_exprod:
			// shift
			push_stack( &Parser::state_43, &Parser::gotof_43, value );
			return false;
		case token_mul:
			// shift
			push_stack( &Parser::state_41, &Parser::gotof_41, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_mod:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_mod( 2, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_33( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_33( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_arg( 1, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_34( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_34( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
			// shift
			push_stack( &Parser::state_8, &Parser::gotof_8, value );
			return false;
		case token_r_bracket:
			// shift
			push_stack( &Parser::state_66, &Parser::gotof_66, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_35( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_35( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
			// shift
			push_stack( &Parser::state_8, &Parser::gotof_8, value );
			return false;
		case token_r_broket:
			// shift
			push_stack( &Parser::state_67, &Parser::gotof_67, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_36( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_36( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
			// shift
			push_stack( &Parser::state_8, &Parser::gotof_8, value );
			return false;
		case token_r_pare:
			// shift
			push_stack( &Parser::state_72, &Parser::gotof_72, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_37( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_37( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
			// shift
			push_stack( &Parser::state_8, &Parser::gotof_8, value );
			return false;
		case token_r_bracket:
			// shift
			push_stack( &Parser::state_83, &Parser::gotof_83, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_38( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_38( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
			// shift
			push_stack( &Parser::state_8, &Parser::gotof_8, value );
			return false;
		case token_r_bracket:
			// shift
			push_stack( &Parser::state_85, &Parser::gotof_85, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_39( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_39( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
			return call_0_make_args( 1, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_40( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_40( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_term( 9, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_41( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 11: return push_stack( &Parser::state_42, &Parser::gotof_42, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_41( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_42( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_42( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_mul( 9, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_43( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 11: return push_stack( &Parser::state_44, &Parser::gotof_44, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_43( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_44( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_44( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_exprod( 9, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_45( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 11: return push_stack( &Parser::state_46, &Parser::gotof_46, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_45( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_46( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_46( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_div( 9, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_47( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 11: return push_stack( &Parser::state_48, &Parser::gotof_48, v );
		case 7: return push_stack( &Parser::state_49, &Parser::gotof_49, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_47( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_50, &Parser::gotof_50, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_48( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_48( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_dot( 9, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_49( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_49( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_pow:
			// shift
			push_stack( &Parser::state_53, &Parser::gotof_53, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_unary_operation_factor( 11, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_50( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 7: return push_stack( &Parser::state_51, &Parser::gotof_51, v );
		case 3: return push_stack( &Parser::state_52, &Parser::gotof_52, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_50( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_51( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_51( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_pow:
			// shift
			push_stack( &Parser::state_53, &Parser::gotof_53, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_negate( 11, 2, 1 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_52( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_52( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_exclamation:
			// shift
			push_stack( &Parser::state_58, &Parser::gotof_58, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_middle_factor( 7, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_53( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 3: return push_stack( &Parser::state_54, &Parser::gotof_54, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_53( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_55, &Parser::gotof_55, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_54( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_54( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_exclamation:
			// shift
			push_stack( &Parser::state_58, &Parser::gotof_58, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_pow( 7, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_55( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 3: return push_stack( &Parser::state_56, &Parser::gotof_56, v );
		case 8: return push_stack( &Parser::state_57, &Parser::gotof_57, v );
		case 4: return push_stack( &Parser::state_59, &Parser::gotof_59, v );
		default: assert(0); return false;
		}
	}

	bool state_55( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_56( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_56( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_exclamation:
			// shift
			push_stack( &Parser::state_58, &Parser::gotof_58, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_pow_negate( 7, 4, 0, 3 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_57( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_57( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_rational:
			// shift
			push_stack( &Parser::state_60, &Parser::gotof_60, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_fact_factor( 3, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_58( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_58( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_sub:
			return call_0_make_fact( 3, 2, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_59( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_59( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_rational_factor( 8, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_60( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 4: return push_stack( &Parser::state_61, &Parser::gotof_61, v );
		default: assert(0); return false;
		}
	}

	bool state_60( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		case token_sub:
			// shift
			push_stack( &Parser::state_62, &Parser::gotof_62, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_61( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_61( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_rational( 8, 3, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_62( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 4: return push_stack( &Parser::state_63, &Parser::gotof_63, v );
		default: assert(0); return false;
		}
	}

	bool state_62( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_built_in_type:
			// shift
			push_stack( &Parser::state_73, &Parser::gotof_73, value );
			return false;
		case token_float_precision:
			// shift
			push_stack( &Parser::state_78, &Parser::gotof_78, value );
			return false;
		case token_id:
			// shift
			push_stack( &Parser::state_71, &Parser::gotof_71, value );
			return false;
		case token_integer:
			// shift
			push_stack( &Parser::state_64, &Parser::gotof_64, value );
			return false;
		case token_l_brace:
			// shift
			push_stack( &Parser::state_68, &Parser::gotof_68, value );
			return false;
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_3, &Parser::gotof_3, value );
			return false;
		case token_l_broket:
			// shift
			push_stack( &Parser::state_4, &Parser::gotof_4, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_12, &Parser::gotof_12, value );
			return false;
		case token_real:
			// shift
			push_stack( &Parser::state_65, &Parser::gotof_65, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_63( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_63( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_rational_negate( 8, 4, 0, 3 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_64( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_64( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_integer( 4, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_65( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_65( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_real( 4, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_66( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_66( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_vector( 4, 3, 1 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_67( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_67( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_c_vector( 4, 3, 1 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_68( int nonterminal_index, const value_type& v )
	{
		switch( nonterminal_index ) {
		case 6: return push_stack( &Parser::state_69, &Parser::gotof_69, v );
		default: assert(0); return false;
		}
	}

	bool state_68( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_6, &Parser::gotof_6, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_69( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_69( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
			// shift
			push_stack( &Parser::state_84, &Parser::gotof_84, value );
			return false;
		case token_r_brace:
			// shift
			push_stack( &Parser::state_70, &Parser::gotof_70, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_70( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_70( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_matrix( 4, 3, 1 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_71( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_71( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_l_pare:
			// shift
			push_stack( &Parser::state_5, &Parser::gotof_5, value );
			return false;
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_identifier( 4, 1, 0 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_72( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_72( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_function( 4, 4, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_73( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_73( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_l_broket:
			// shift
			push_stack( &Parser::state_10, &Parser::gotof_10, value );
			return false;
		case token_l_pare:
			// shift
			push_stack( &Parser::state_9, &Parser::gotof_9, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_74( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_74( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_r_pare:
			// shift
			push_stack( &Parser::state_75, &Parser::gotof_75, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_75( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_75( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_cast( 4, 4, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_76( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_76( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_r_broket:
			// shift
			push_stack( &Parser::state_77, &Parser::gotof_77, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_77( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_77( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_integer_cast( 4, 4, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_78( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_78( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_l_pare:
			// shift
			push_stack( &Parser::state_11, &Parser::gotof_11, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_79( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_79( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_r_pare:
			// shift
			push_stack( &Parser::state_80, &Parser::gotof_80, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_80( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_80( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_precision_cast( 4, 4, 0, 2 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_81( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_81( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_r_pare:
			// shift
			push_stack( &Parser::state_82, &Parser::gotof_82, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_82( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_82( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_eof:
		case token_add:
		case token_comma:
		case token_div:
		case token_dot:
		case token_exclamation:
		case token_exprod:
		case token_mod:
		case token_mul:
		case token_pow:
		case token_r_allow:
		case token_r_bracket:
		case token_r_broket:
		case token_r_pare:
		case token_rational:
		case token_sub:
			return call_0_make_factor_expr( 4, 3, 1 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_83( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_83( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
		case token_r_brace:
			return call_0_make_matrix_term( 6, 3, 1 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_84( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_84( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_l_bracket:
			// shift
			push_stack( &Parser::state_7, &Parser::gotof_7, value );
			return false;
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

	bool gotof_85( int nonterminal_index, const value_type& v )
	{
		assert(0);
		return true;
	}

	bool state_85( token_type token, const value_type& value )
	{
		switch( token ) {
		case token_comma:
		case token_r_brace:
			return call_0_make_matrix_terms( 6, 5, 0, 3 );
		default:
			sa_.syntax_error(stack_);
			error_ = true;
			return false;
		}
	}

};

} // namespace calc

#endif // #ifndef PARSER_HPP_
