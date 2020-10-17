#define BOOST_TEST_MODULE tests
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/test/data/monomorphic.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/random_generator.hpp>
#include <chrono>
#include <cmath>
#include <ctime>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <unordered_map>
#pragma comment(lib, "BCrypt.lib")
#pragma comment(lib, "shlwapi.lib")

namespace bdata = boost::unit_test::data;

BOOST_AUTO_TEST_CASE(RawGuidTest)
{
    boost::uuids::random_generator_mt19937 uuidGen;
    std::vector<boost::uuids::uuid> contained;
    for (int i = 0; i < 10; ++i)
    {
        auto u = uuidGen();
        contained.push_back(u);
    }
    std::vector<boost::uuids::uuid> notContained;
    for (int i = 0; i < 10; ++i) {
        auto u = uuidGen();
        notContained.push_back(u);
    }

}
