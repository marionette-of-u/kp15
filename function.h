#ifndef KP15_FUNCTION_H
#define KP15_FUNCTION_H

#include "header.h"

namespace kp15{
	//éñëOÇ…ln(2), sqrt(2)ÇåvéZÇµÇƒÇ®Ç≠
	extern mpf_class mpf_log2, mpf_sqrt2;
	void calc_log2(mp_bitcnt_t);

	//éñëOÇ…PI, 3127 / 2048 - PI / 2 = dÇåvéZÇµÇƒÇ®Ç≠
	extern mpf_class mpf_pi, mpf_3127_div_2048, mpf_pi_d;
	extern mp_bitcnt_t current_mp_bitcnt_pi;
	void calc_pi(mp_bitcnt_t);

	//ëŒêîä÷êî
	mpf_class log(mpf_class);

	//éwêîä÷êî
	mpf_class exp(mpf_class);

	//ê‚ëŒíl
	mpz_class abs(const mpz_class&);
	mpf_class abs(const mpf_class&);
	mpq_class abs(const mpq_class&);

	//éOäpä÷êî
	mpf_class tan(const mpf_class&);
	mpf_class sin(const mpf_class&);
	mpf_class cos(const mpf_class&);
	mpf_class atan(mpf_class);
	mpf_class acos(const mpf_class&);
	mpf_class asin(const mpf_class&);
	mpf_class csc(const mpf_class&);
	mpf_class sec(const mpf_class&);
	mpf_class cot(const mpf_class&);

	//ëoã»ê¸ä÷êî
	mpf_class sinh(const mpf_class&);
	mpf_class cosh(const mpf_class&);
	mpf_class tanh(const mpf_class&);
	mpf_class asinh(const mpf_class&);
	mpf_class acosh(const mpf_class&);
	mpf_class atanh(const mpf_class&);

	//ó›èÊ
	mpz_class pow_zzz(const mpz_class&, const mpz_class&);
	mpq_class pow_qzz(const mpz_class&, const mpz_class&);
	mpf_class pow_fff(const mpf_class&, const mpf_class&);
	mpf_class pow(const mpz_class&, const mpf_class&);
	mpf_class pow(const mpz_class&, const mpq_class&);
	mpf_class pow(const mpf_class&, const mpz_class&);
	mpf_class pow(const mpf_class&, const mpf_class&);
	mpf_class pow(const mpf_class&, const mpq_class&);
	mpf_class pow(const mpq_class&, const mpz_class&);
	mpf_class pow(const mpq_class&, const mpf_class&);
	mpf_class pow(const mpq_class&, const mpq_class&);

	//äKèÊ
	mpz_class fact_zz(const mpz_class&);
	mpf_class fact_ff(const mpf_class&);

	//ÉKÉìÉ}ä÷êî
	mpf_class log_gamma(mpf_class);
	mpf_class gamma(const mpf_class&);

	//gcd, lcm
	mpz_class gcd(mpz_class, mpz_class);
	mpz_class lcm(mpz_class, mpz_class);
}

#endif //KP15_FUNCTION_H
