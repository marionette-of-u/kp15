#include "function.h"
#include <limits>

namespace kp15{
	mpf_class mpf_log2, mpf_sqrt2;
	namespace{ mp_bitcnt_t current_mp_bitcnt_log2 = 0; }
	void calc_log2(mp_bitcnt_t prec){
		if(prec == current_mp_bitcnt_log2){ return; }
		mpf_log2.set_prec(prec);
		mpf_sqrt2.set_prec(prec);
		current_mp_bitcnt_log2 = mpf_log2.get_prec();
		mpf_log2 = 0;
		mpf_class u, u_;
		u.set_prec(current_mp_bitcnt_log2), u_.set_prec(current_mp_bitcnt_log2);
		{
			mpf_class one, three;
			one.set_prec(current_mp_bitcnt_log2), three.set_prec(current_mp_bitcnt_log2);
			one = 1, three = 3;
			u = u_ = one / three;
		}
		int i = 1;
		for(mp_bitcnt_t c = 0; c < current_mp_bitcnt_log2 / 3 + 1; ++c){
			mpf_log2 += 2 * u / i;
			for(int j = 0; j < 2; ++j){ u *= u_; }
			i += 2;
		}
		mpf_sqrt2 = 2;
		mpf_sqrt2 = sqrt(mpf_sqrt2);
	}

	mpf_class mpf_pi, mpf_3127_div_2048, mpf_pi_d;
	mp_bitcnt_t current_mp_bitcnt_pi = 0;
	void calc_pi(mp_bitcnt_t prec){
		if(prec == current_mp_bitcnt_pi){ return; }
		current_mp_bitcnt_pi = prec;

		//pi
		{
			mpf_pi.set_prec(current_mp_bitcnt_pi), mpf_pi_d.set_prec(current_mp_bitcnt_pi);
			calc_log2(prec);
			mpf_class a, b, s, t, last, temp;
			a.set_prec(prec), b.set_prec(prec), s.set_prec(prec), t.set_prec(prec), last.set_prec(prec), temp.set_prec(prec);
			a = 1, b = 1 / mpf_sqrt2, s = 1, t = 4;
			for(std::size_t i = 0, n = (prec / 64 + 1) * 4; i < n; ++i){
				last = a, a = (a + b) / 2; b = sqrt(last * b);
				temp = a - last;
				s -= t * temp * temp;
				t.dexp(0, 1);
			}
			temp = a + b;
			mpf_pi = temp * temp / s;
		}

		//d = 3127 / 2048 - PI / 2
		{
			mpf_class v3127;
			v3127.set_prec(prec), mpf_3127_div_2048.set_prec(prec);
			v3127 = 3127;
			mpf_3127_div_2048 = v3127 / 2048;
			mpf_pi_d = mpf_3127_div_2048 - mpf_pi / 2;
		}
	}

	mpf_class log(mpf_class x){
		if(x <= 0){
			throw(std::string("‘Î”ŠÖ”‚Ìˆø”‚ª–³Œø‚È’l‚Å‚·. "));
		}
		bool inverse = x < 1;
		if(inverse){ x = 1 / x; }
		mp_bitcnt_t prec_bit = x.get_prec();
		calc_log2(prec_bit);
		mpf_class s;
		s.set_prec(x.get_prec());
		int k = mpf_class(x / mpf_sqrt2).r2_exp();
		{
			mpz_class k_ = 1;
			k_ <<= k;
			x /= k_;
		}
		x -= 1;
		unsigned int n = (prec_bit / 32 + 1) * 5;
		for(unsigned int i = n; i >= 1; --i){
			s = i * x / (2 + i * x / (2 * i + 1 + s));
		}
		if(inverse){
			return -(mpf_log2 * k + x / (1 + s));
		}else{
			return mpf_log2 * k + x / (1 + s);
		}
	}

	mpf_class exp(mpf_class x){
		if(x == 0){ return mpf_class(1); }
		bool inverse = x < 0;
		if(inverse){ x = -x; }
		mp_bitcnt_t prec_bit = x.get_prec(), n = (prec_bit / 128 + 1) * 50;
		calc_log2(prec_bit);
		mpz_class k;
		mpf_class x2, w, tmp, half;
		x2.set_prec(prec_bit), w.set_prec(prec_bit), tmp.set_prec(prec_bit), half.set_prec(prec_bit);
		half = 0.5;
		k = x / mpf_log2 + (x >= 0 ? half : -half);
		x -= k * mpf_log2;
		x2 = x * x; w = x2 / n;
		for(mp_bitcnt_t i = n - 4; i >= 6; i -= 4){ w = x2 / (w + i); }
		tmp = (2 + w + x) / (2 + w - x);
		unsigned int l = mpz_class(k / 32).get_ui(), m = mpz_class(k % 32).get_ui();
		if(l != 0 || m != 0){
			tmp.dexp(l, m);
			return inverse ? 1 / tmp : tmp;
		}else{
			return 1 / tmp;
		}
	}

	mpz_class abs(const mpz_class &arg){
		if(arg < 0){
			return -arg;
		}else{
			return arg;
		}
	}

	mpf_class abs(const mpf_class &arg){
		if(arg < 0){
			return -arg;
		}else{
			return arg;
		}
	}

	mpq_class abs(const mpq_class &arg){
		if(arg < 0){
			return -arg;
		}else{
			return arg;
		}
	}

	namespace{
		mpf_class ur_tan(mpf_class x, unsigned int &k){
			mp_bitcnt_t prec = x.get_prec();
			calc_pi(prec);
			mpf_class t, x2;
			t.set_prec(prec), x2.set_prec(prec);
			k = mpf_class(x / (mpf_pi / 2) + (x >= 0 ? 0.5 : -0.5)).get_ui();
			x = (x - mpf_3127_div_2048 * k) + mpf_pi_d * k;
			x2 = x * x, t = 0;
			std::size_t n = (prec / 64 + 1) * 21;
			for(std::size_t i = n; i >= 3; i -= 2){ t = x2 / (i - t); }
			return x / (1 - t);
		}
	}

	mpf_class tan(const mpf_class &x){
		unsigned int k;
		mpf_class t;
		t.set_prec(x.get_prec());
		t = ur_tan(x, k);
		if(k % 2 == 0){
			return t;
		}else if(t != 0){
			return -1 / t;
		}
		return HUGE_VAL;
	}

	mpf_class sin(const mpf_class &x){
		unsigned int k;
		mpf_class t;
		t.set_prec(x.get_prec());
		t = ur_tan(x / 2, k);
		t = t * 2 / (1 + t * t);
		if(k % 2 == 0){
			return t;
		}else{
			return -t;
		}
	}

	mpf_class cos(const mpf_class &x){
		return sin(mpf_pi / 2 - abs(x));
	}

	mpf_class atan(mpf_class x){
		mp_bitcnt_t prec = x.get_prec();
		calc_pi(prec);
		int sign;
		mpf_class a;
		a.set_prec(prec);
		if(x > 1){
			sign = 1;
			x = 1 / x;
		}else if(x < -1){
			sign = -1;
			x = 1 / x;
		}else{
			sign = 0;
		}
		a = 0;
		std::size_t n = (prec / 64 + 1) * 24;
		for(std::size_t i = n; i >= 1; --i){
			a = (i * i * x * x) / (2 * i + 1 + a);
		}
		if(sign > 0){
			return mpf_pi / 2 - x / (1 + a);
		}else if(sign < 0){
			return -mpf_pi / 2 - x / (1 + a);
		}else{
			return x / (1 + a);
		}
	}

	mpf_class acos(const mpf_class &x){
		return mpf_pi / 2 - asin(x);
	}

	mpf_class asin(const mpf_class &x){
		mpf_class temp;
		temp.set_prec(x.get_prec());
		temp = 1 - x * x;
		return atan(x / pow_fff(temp, mpf_class(0.5)));
	}

	mpf_class sinh(const mpf_class &x){
		mpf_class t = exp(x);
		return x > 0 ? kp15::abs(mpf_class((-1 / t + t) / 2)) : -kp15::abs(mpf_class((-1 / t + t) / 2));
	}

	mpf_class cosh(const mpf_class &x){
		mpf_class t = exp(x);
		return (t + 1 / t) / 2;
	}

	mpf_class tanh(const mpf_class &x){
		if(x > 0){ return 2 / (1 + exp(-2 * x)) - 1; }
		return 1 - 2 / (exp(2 * x) + 1);
	}

	mpf_class asinh(const mpf_class &x){
		if(x > 0){ return  log(pow_fff(x * x + 1, 0.5) + x); }
		else     { return -log(pow_fff(x * x + 1, 0.5) - x); }
	}

	mpf_class acosh(const mpf_class &x){
		return log(x + pow_fff(x * x - 1, 0.5));
	}

	mpf_class atanh(const mpf_class &x){
		return 0.5 * log((1 + x) / (1 - x));
	}

	mpf_class csc(const mpf_class &x){
		return 1 / sin(x);
	}

	mpf_class sec(const mpf_class &x){
		return 1 / cos(x);
	}

	mpf_class cot(const mpf_class &x){
		return 1 / tan(x);
	}

	mpz_class pow_zzz(const mpz_class &x, const mpz_class &y){
		mpz_class r = 1;
		for(mpz_class i = 0; i < y; i += 1){
			r *= x;
		}
		return r;
	}

	mpq_class pow_qzz(const mpz_class &x, const mpz_class &y){
		mpq_class r = 1;
		for(mpz_class i = 0; i > y; i -= 1){
			r /= x;
		}
		return r;
	}

	mpf_class pow_fff(const mpf_class &x, const mpf_class &y){
		return exp(y * log(x));
	}

	mpf_class pow(const mpz_class &x, const mpf_class &y){
		return pow_fff(mpf_class(x), y);
	}

	mpf_class pow(const mpz_class &x, const mpq_class &y){
		return pow_fff(mpf_class(x), mpf_class(y));
	}

	mpf_class pow(const mpf_class &x, const mpz_class &y){
		return pow_fff(x, mpf_class(y));
	}

	mpf_class pow(const mpf_class &x, const mpf_class &y){
		return pow_fff(x, y);
	}

	mpf_class pow(const mpf_class &x, const mpq_class &y){
		return pow_fff(x, mpf_class(y));
	}

	mpf_class pow(const mpq_class &x, const mpz_class &y){
		return pow_fff(mpf_class(x), mpf_class(y));
	}

	mpf_class pow(const mpq_class &x, const mpq_class &y){
		return pow_fff(mpf_class(x), mpf_class(y));
	}


	mpf_class pow(const mpq_class &x, const mpf_class &y){
		return pow_fff(mpf_class(x), y);
	}

	mpz_class fact_zz(const mpz_class &x){
		if(x < 0){ throw(std::string("'fact' •‰”‚ÌŠKæ‚Å‚·. ")); }
		mpz_class r = 1;
		for(mpz_class i = x; i > 1; --i){
			r *= i;
		}
		return r;
	}

	mpf_class fact_ff(const mpf_class &x){
		if(x < 0){ throw(std::string("'fact' •‰”‚ÌŠKæ‚Å‚·. ")); }
		return gamma(x + 1.0);
	}

	mpf_class log_gamma(mpf_class x){
		mp_bitcnt_t prec = x.get_prec();
		calc_pi(prec);
		mpf_class v1, v2, v5, v6, v7, v30, v42, v66, v510, v691, v2730, v3617;
		v1.set_prec(prec), v1 = 1,
		v2.set_prec(prec), v2 = 2,
		v5.set_prec(prec), v5 = 5,
		v6.set_prec(prec), v6 = 6,
		v7.set_prec(prec), v7 = 7,
		v30.set_prec(prec), v30 = 30,
		v42.set_prec(prec), v42 = 42,
		v66.set_prec(prec), v66 = 66,
		v510.set_prec(prec), v510 = 510,
		v691.set_prec(prec), v691 = 691,
		v2730.set_prec(prec), v2730 = 2730,
		v3617.set_prec(prec), v3617 = 3617;
		mpf_class b0, b1, b2, b4, b6, b8, b10, b12, b14, b16;
		b0.set_prec(prec), b0 = v1,
		b1.set_prec(prec), b1 = -v1 / v2,
		b2.set_prec(prec), b2 = v1 / v6,
		b4.set_prec(prec), b4 = -v1 / v30,
		b6.set_prec(prec), b6 = v1 / v42,
		b8.set_prec(prec), b8 = -v1 / v30,
		b10.set_prec(prec), b10 = v5 / v66,
		b12.set_prec(prec), b12 = -v691 / v2730,
		b14.set_prec(prec), b14 = v7 / v6,
		b16.set_prec(prec), b16 = -v3617 / v510;
		mpf_class v, w, log2pi, temp;
		v.set_prec(prec), w.set_prec(prec), log2pi.set_prec(prec), temp.set_prec(prec);
		v = 1;
		while(x < 8){ v *= x; x += 1; }
		w = 1 / (x * x);
		log2pi = log(mpf_pi * 2);
		temp = (((b16 / (16 * 15))  * w + (b14 / (14 * 13))) * w + (b12 / (12 * 11)));
		temp = ((temp * w + (b10 / (10 *  9))) * w + (b8  / ( 8 *  7)));
		temp = ((temp * w + (b6  / ( 6 *  5))) * w + (b4  / ( 4 *  3)));
		return (temp * w + (b2  / ( 2 *  1))) / x + 0.5 * log2pi - log(v) - x + (x - 0.5) * log(x);
		//return ((((((((b16 / (16 * 15))  * w + (b14 / (14 * 13))) * w
		//	        + (b12 / (12 * 11))) * w + (b10 / (10 *  9))) * w
		//			+ (b8  / ( 8 *  7))) * w + (b6  / ( 6 *  5))) * w
		//			+ (b4  / ( 4 *  3))) * w + (b2  / ( 2 *  1))) / x
		//			+ 0.5 * log2pi - log(v) - x + (x - 0.5) * log(x);
	}

	mpf_class gamma(const mpf_class &x){
		mp_bitcnt_t prec = x.get_prec();
		calc_pi(prec);
		if(x < 0){
			return mpf_pi / (sin(mpf_pi * x) * exp(log_gamma(1 - x)));
		}else{
			return exp(log_gamma(x));
		}
	}

	mpz_class gcd(mpz_class x, mpz_class y){
		mpz_class t;
		if(y > x){
			while(y != 0){
				t = x % y; x = y; y = t;
			}
			return x;
		}else{
			while(x != 0){
				t = y % x; y = x; x = t;
			}
			return y;
		}
	}

	mpz_class lcm(mpz_class x, mpz_class y){
		mpz_class r = x * y / gcd(x, y);
		if(r < 0){ r = -r; }
		return r;
	}
}
