#pragma once

#include "parse.h"
#include <boost/lexical_cast.hpp>

namespace kp15{ namespace parse{
	value_holder add_value(const value_holder &l, const value_holder &r);
	value_holder sub_value(const value_holder &l, const value_holder &r);
	value_holder mod_value(const value_holder &l, const value_holder &r);
	value_holder mul_value(const value_holder &l, const value_holder &r);
	value_holder div_value(const value_holder &l, const value_holder &r);
	value_holder dot_value(const value_holder &l, const value_holder &r);
	value_holder exprod_value(const value_holder &l, const value_holder &r);
	value_holder pow_value(const value_holder &l, const value_holder &r);
	value_holder fact_value(const value_holder &f);
	value_holder rational_value(const value_holder &l, const value_holder &r);
	value_holder nagete_value(const value_holder &f);
	value_holder cast_value(const value_holder &f, const std::string &cast_str);
	value_holder precision_cast_value(const value_holder& f);

	value_holder norm_value(const value_holder &f);
	value_holder transpose_value(const value_holder &f);
	bool lt_value(const value_holder &l, const value_holder &r);
	bool gt_value(const value_holder &l, const value_holder &r);
	bool eq_value(const value_holder &l, const value_holder &r);
	bool neq_value(const value_holder &l, const value_holder &r);
} }
