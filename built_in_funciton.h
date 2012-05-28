#ifndef KP15_BUILT_IN_FUNCTION_H
#define KP15_BUILT_IN_FUNCTION_H

#include<boost/unordered_map.hpp>
#include "header.h"
#include "parse.h"

namespace kp15{ namespace built_in_function{
	//ëgÇ›çûÇ›ä÷êî
	typedef parse::value_holder (*fn_type)(const parse::value_seq&);

	struct fn_struct{
		std::size_t arg_num;
		fn_type fn;
	};

	typedef boost::unordered_map<std::string, fn_struct> fn_map_type;
	extern fn_map_type fn_map;
} }

#endif KP15_BUILT_IN_FUNCTION_H
