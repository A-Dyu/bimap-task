#include "bimap.h"

#include "gtest/gtest.h"
#include <random>

struct test_object {
    int a = 0;
    test_object() = default;
    explicit test_object(int b) : a(b) {}
    test_object(test_object &&other) noexcept { std::swap(a, other.a); }
    friend bool operator<(test_object const &c, test_object const &b) {
        return c.a < b.a;
    }
    friend bool operator==(test_object const &c, test_object const &b) {
        return c.a == b.a;
    }
};

TEST(bimap, leak_check) {
    bimap<int, int> b;

    std::mt19937 e(std::random_device{}());
    for (size_t i = 0; i < 10000; i++) {
        b.insert(e(), e());
    }
}

TEST(bimap, simple) {
    bimap<int, int> b;
    b.insert(4, 4);
    EXPECT_EQ(b.at_right(4), b.at_left(4));
}

TEST(bimap, custom_comparator) {
    bimap<int, int, std::greater<>> b;
    b.insert(3, 4);
    b.insert(1, 5);
    b.insert(10, -10);

    int prev = *b.begin_left();
    for (auto it = ++b.begin_left(); it != b.end_left(); it++) {
        EXPECT_GT(prev, *it);
    }
    prev = *b.begin_right();
    for (auto it = ++b.begin_right(); it != b.end_right(); it++) {
        EXPECT_LT(prev, *it);
    }
}

TEST(bimap, copies) {
    bimap<int, int> b;
    b.insert(3, 4);
    bimap<int, int> b1(b);
    EXPECT_EQ(*b.find_left(3).flip(), 4);
    b1.insert(4, 5);
    EXPECT_EQ(b.find_left(4), b.end_left());

    b1.insert(10, -10);
    b = b1;
    EXPECT_NE(b.find_right(-10), b.end_right());
}

TEST(bimap, insert) {
    bimap<int, int> b;
    b.insert(4, 10);
    b.insert(10, 4);
    EXPECT_EQ(*b.find_right(4).flip(), 10);
    EXPECT_EQ(b.at_left(10), 4);
}

TEST(bimap, insert_move) {
    bimap<int, test_object> b;
    test_object x(3), x2(3);
    b.insert(4, std::move(x));
    EXPECT_EQ(x.a, 0);
    EXPECT_EQ(b.at_right(x2), 4);
    EXPECT_EQ(b.at_left(4), x2);

    bimap<test_object, int> b2;
    test_object y(4), y2(4);
    b2.insert(std::move(y), 3);
    EXPECT_EQ(y.a, 0);
    EXPECT_EQ(b2.at_left(y2), 3);
    EXPECT_EQ(b2.at_right(3), y2);
}

TEST(bimap, at) {
    bimap<int, int> b;
    b.insert(4, 3);

    EXPECT_THROW(b.at_left(1), std::out_of_range);
    EXPECT_THROW(b.at_right(300), std::out_of_range);
    EXPECT_EQ(b.at_left(4), 3);
    EXPECT_EQ(b.at_right(3), 4);
}

TEST(bimap, find) {
    bimap<int, int> b;
    b.insert(3, 4);
    b.insert(4, 5);
    b.insert(42, 1000);

    EXPECT_EQ(*b.find_right(5).flip(), 4);
    EXPECT_EQ(*b.find_left(3).flip(), 4);
    EXPECT_EQ(b.find_left(3436), b.end_left());
    EXPECT_EQ(b.find_right(-1000), b.end_right());
}

TEST(bimap, empty) {
    bimap<int, int> b;
    EXPECT_TRUE(b.empty());
    b.insert(1, 1);
    EXPECT_FALSE(b.empty());
}

TEST(bimap, insert_exist) {
    bimap<int, int> b;
    b.insert(1, 2);
    b.insert(2, 3);
    b.insert(3, 4);
    EXPECT_EQ(b.size(), 3);
    auto it = b.insert(2, -1);
    EXPECT_EQ(it, b.end_left());
    EXPECT_EQ(b.size(), 3);
}

TEST(bimap, erase_iterator) {
    bimap<int, int> b;
    auto it = b.insert(1, 2);
    b.insert(5, 10);
    b.insert(100, 200);
    auto it1 = b.erase_left(it);
    EXPECT_EQ(b.size(), 2);
    EXPECT_EQ(*it1, 5);

    it = b.insert(-1, -2);
    auto itr = b.erase_right(it.flip());
    EXPECT_EQ(b.size(), 2);
    EXPECT_EQ(*itr, 10);
}

TEST(bimap, erase_value) {
    bimap<int, int> b;

    b.insert(111, 222);
    b.insert(333, 444);
    EXPECT_TRUE(b.erase_left(111));
    EXPECT_EQ(b.size(), 1);
    EXPECT_FALSE(b.erase_right(333333));
    EXPECT_EQ(b.size(), 1);
}

TEST(bimap, erase_range) {
    bimap<int, int> b;

    b.insert(1, 2);
    auto f = b.insert(2, 3);
    b.insert(3, 4);
    auto l = b.insert(4, 5);
    b.insert(5, 6);

    auto it = b.erase_left(f, l);
    EXPECT_EQ(*it, 4);
    EXPECT_EQ(b.size(), 3);

    auto f1 = b.insert(100, 4).flip();
    auto l1 = b.insert(200, 10).flip();

    auto it1 = b.erase_right(f1, l1);
    EXPECT_EQ(*it1, 10);
    EXPECT_EQ(b.size(), 2);

    b.erase_left(b.begin_left(), b.end_left());
    EXPECT_TRUE(b.empty());
}

TEST(bimap, lower_bound) {
    bimap<int, int> b;

    std::vector<std::pair<int, int>> data = {
            {1, 2}, {2, 3}, {3, 4}, {8, 16}, {32, 66}};

    std::shuffle(data.begin(), data.end(), std::random_device{});
    for (auto const &p : data) {
        b.insert(p.first, p.second);
    }

    EXPECT_EQ(*b.lower_bound_left(5), 8);
    EXPECT_EQ(*b.lower_bound_right(4), 4);
    EXPECT_EQ(*b.lower_bound_left(4).flip(), 16);
    EXPECT_EQ(b.lower_bound_right(100), b.end_right());
    EXPECT_EQ(b.lower_bound_left(100), b.end_left());
}

TEST(bimap, upper_bound) {
    bimap<int, int> b;

    std::vector<std::pair<int, int>> data = {
            {1, 2}, {2, 3}, {3, 4}, {8, 16}, {32, 66}};

    std::shuffle(data.begin(), data.end(), std::random_device{});
    for (auto const &p : data) {
        b.insert(p.first, p.second);
    }

    EXPECT_EQ(*b.upper_bound_left(5), 8);
    EXPECT_EQ(*b.upper_bound_right(-100), 2);
    EXPECT_EQ(b.upper_bound_right(100), b.end_right());
    EXPECT_EQ(b.upper_bound_left(400), b.end_left());
}

template <typename T>
std::vector<std::pair<T, T>>
eliminate_same(std::vector<T> &lefts, std::vector<T> &rights, std::mt19937 &e) {
    // std::sort(lefts.begin(), lefts.end());
    auto last = std::unique(lefts.begin(), lefts.end());
    lefts.erase(last, lefts.end());
    last = std::unique(rights.begin(), rights.end());
    rights.erase(last, rights.end());

    size_t min = std::min(lefts.size(), rights.size());
    lefts.resize(min);
    rights.resize(min);

    std::shuffle(lefts.begin(), lefts.end(), e);
    std::shuffle(rights.begin(), rights.end(), e);

    std::vector<std::pair<T, T>> res(min);
    for (size_t i = 0; i < min; i++) {
        res[i] = {lefts[i], rights[i]};
    }

    return res;
}

static constexpr uint32_t seed = 1488228;

TEST(bimap_randomized, comparison) {
    std::cout << "Seed used for randomized compare test is " << seed << std::endl;

    bimap<uint32_t, uint32_t> b1;
    bimap<uint32_t, uint32_t> b2;

    size_t total = 40000;
    std::mt19937 e(seed);
    std::vector<uint32_t> lefts(total), rights(total);
    for (size_t i = 0; i < total; i++) {
        lefts[i] = e();
        rights[i] = e();
    }
    auto future_insertions = eliminate_same(lefts, rights, e);

    std::shuffle(future_insertions.begin(), future_insertions.end(), e);
    for (auto p : future_insertions) {
        b1.insert(p.first, p.second);
    }

    std::shuffle(future_insertions.begin(), future_insertions.end(), e);
    for (auto p : future_insertions) {
        b2.insert(p.first, p.second);
    }

    EXPECT_EQ(b1.size(), b2.size());
    EXPECT_EQ(b1, b2);
}

TEST(bimap_randomized, invariant_check) {
    std::cout << "Seed used for randomized invariant test is " << seed
              << std::endl;
    bimap<int, int> b;

    std::mt19937 e(seed);
    size_t ins = 0, skip = 0, total = 50000;
    for (size_t i = 0; i < total; i++) {
        auto op = e() % 10;
        if (op > 2) {
            ins++;
            b.insert(e(), e());
        } else {
            if (b.empty()) {
                skip++;
                continue;
            }
            auto it = b.end_left();
            while (it == b.end_left()) {
                it = b.lower_bound_left(e());
            }
            b.erase_left(it);
        }
        if (i % 100 == 0) {
            int previous = *b.begin_left();
            for (auto it = ++b.begin_left(); it != b.end_left(); it++) {
                EXPECT_GT(*it, previous);
                previous = *it;
            }
            previous = *b.begin_right();
            for (auto it = ++b.begin_right(); it != b.end_right(); it++) {
                EXPECT_GT(*it, previous);
                previous = *it;
            }
        }
    }
    std::cout << "Invariant check stats:" << std::endl;
    std::cout << "Performed " << ins << " insertions and " << total - ins - skip
              << " erasures. " << skip << " skipped." << std::endl;
}

TEST(bimap_randomized, compare_to_two_maps) {
    std::cout << "Seed used for randomized cmp2map test is " << seed << std::endl;

    bimap<int, int> b;
    std::map<int, int> left_view, right_view;

    std::mt19937 e(seed);
    size_t ins = 0, skip = 0, total = 60000;
    for (size_t i = 0; i < total; i++) {
        unsigned int op = e() % 10;
        if (op > 2) {
            ins++;
            // insertion
            int l = e(), r = e();
            b.insert(l, r);
            left_view.insert({l, r});
            right_view.insert({r, l});
        } else {
            // erasure
            if (b.empty()) {
                skip++;
                continue;
            }
            auto it = b.end_left();
            while (it == b.end_left()) {
                it = b.lower_bound_left(e());
            }
            EXPECT_EQ(left_view.erase(*it), 1);
            EXPECT_EQ(right_view.erase(*it.flip()), 1);
            b.erase_left(it);
        }
        if (i % 100 == 0) {
            // check
            EXPECT_EQ(b.size(), left_view.size());
            EXPECT_EQ(b.size(), right_view.size());
            auto lit = b.begin_left();
            auto mlit = left_view.begin();
            for (; lit != b.end_left() && mlit != left_view.end(); lit++, mlit++) {
                EXPECT_EQ(*lit, mlit->first);
                EXPECT_EQ(*lit.flip(), mlit->second);
            }
        }
    }
    std::cout << "Comparing to maps stat:" << std::endl;
    std::cout << "Performed " << ins << " insertions and " << total - ins - skip
              << " erasures. " << skip << " skipped." << std::endl;
}
