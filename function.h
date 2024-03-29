#ifndef KP15_FUNCTION_H
#define KP15_FUNCTION_H

#include "header.h"

namespace kp15{
	//事前にln(2), sqrt(2)を計算しておく
	extern mpf_class mpf_log2, mpf_sqrt2;
	void calc_log2(mp_bitcnt_t);

	//事前にPI, 3127 / 2048 - PI / 2 = dを計算しておく
	extern mpf_class mpf_pi, mpf_3127_div_2048, mpf_pi_d;
	extern mp_bitcnt_t current_mp_bitcnt_pi;
	void calc_pi(mp_bitcnt_t);

	//対数関数
	mpf_class log(mpf_class);

	//指数関数
	mpf_class exp(mpf_class);

	//絶対値
	mpz_class abs(const mpz_class&);
	mpf_class abs(const mpf_class&);
	mpq_class abs(const mpq_class&);

	//三角関数
	mpf_class tan(const mpf_class&);
	mpf_class sin(const mpf_class&);
	mpf_class cos(const mpf_class&);
	mpf_class atan(mpf_class);
	mpf_class acos(const mpf_class&);
	mpf_class asin(const mpf_class&);
	mpf_class csc(const mpf_class&);
	mpf_class sec(const mpf_class&);
	mpf_class cot(const mpf_class&);

	//双曲線関数
	mpf_class sinh(const mpf_class&);
	mpf_class cosh(const mpf_class&);
	mpf_class tanh(const mpf_class&);
	mpf_class asinh(const mpf_class&);
	mpf_class acosh(const mpf_class&);
	mpf_class atanh(const mpf_class&);

	//累乗
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

	//階乗
	mpz_class fact_zz(const mpz_class&);
	mpf_class fact_ff(const mpf_class&);

	//ガンマ関数
	mpf_class log_gamma(mpf_class);
	mpf_class gamma(const mpf_class&);

	//gcd, lcm
	mpz_class gcd(mpz_class, mpz_class);
	mpz_class lcm(mpz_class, mpz_class);
}

#endif //KP15_FUNCTION_H
