#include <iostream>
#include <vector>
#include <algorithm>

typedef std::vector<unsigned> primes_t;

primes_t::value_type next_prime(primes_t &primes)
{
	primes_t::value_type x;
	for(x = primes.back() + 2;
	    std::find_if(primes.begin(), primes.end(),
	                 [x](primes_t::value_type p) { return x % p == 0; })
	    != primes.end();
	    x += 2)
	{
		// do nothing
	}

	return x;
}

int main()
{
	primes_t primes = {2, 3};
	while(primes.size() != 10001)
	{
		primes.push_back(next_prime(primes));
	}
	std::cout << primes.back() << '\n';

	return 0;
}
