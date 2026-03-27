#include "big_integer.h"
#include <algorithm>
#include <cctype>
#include <sstream>
#include <utility>

void BigInteger::normalize() {
    while (digits_.size() > 1 && digits_.back() == 0) {
        digits_.pop_back();
    }
    if (digits_.size() == 1 && digits_[0] == 0) {
        negative_ = false;
    }
}

int BigInteger::compare_abs(const BigInteger& a, const BigInteger& b) {
    if (a.digits_.size() != b.digits_.size()) {
        return a.digits_.size() < b.digits_.size() ? -1 : 1;
    }
    for (size_t i = a.digits_.size(); i-- > 0; ) {
        if (a.digits_[i] != b.digits_[i]) {
            return a.digits_[i] < b.digits_[i] ? -1 : 1;
        }
    }
    return 0;
}

BigInteger BigInteger::add_abs(const BigInteger& a, const BigInteger& b) {
    const std::vector<int>& x = a.digits_;
    const std::vector<int>& y = b.digits_;
    std::vector<int> res;
    int carry = 0;
    size_t i = 0;
    while (i < x.size() || i < y.size() || carry) {
        int sum = carry;
        if (i < x.size()) sum += x[i];
        if (i < y.size()) sum += y[i];
        res.push_back(sum % 10);
        carry = sum / 10;
        ++i;
    }
    // Удаляем возможные ведущие нули (на всякий случай)
    while (res.size() > 1 && res.back() == 0) {
        res.pop_back();
    }
    BigInteger result;
    result.digits_ = res;
    result.negative_ = false;
    return result;
}

BigInteger BigInteger::subtract_abs(const BigInteger& a, const BigInteger& b) {
    const std::vector<int>& x = a.digits_;
    const std::vector<int>& y = b.digits_;
    std::vector<int> res;
    int borrow = 0;
    for (size_t i = 0; i < x.size(); ++i) {
        int diff = x[i] - borrow;
        if (i < y.size()) diff -= y[i];
        if (diff < 0) {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        res.push_back(diff);
    }
    // Удаляем ведущие нули
    while (res.size() > 1 && res.back() == 0) {
        res.pop_back();
    }
    BigInteger result;
    result.digits_ = res;
    result.negative_ = false;
    return result;
}

BigInteger BigInteger::multiply_abs(const BigInteger& a, const BigInteger& b) {
    if (a.is_zero() || b.is_zero()) {
        return BigInteger(0);
    }
    size_t n = a.digits_.size();
    size_t m = b.digits_.size();
    std::vector<long long> temp(n + m, 0);
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            temp[i + j] += static_cast<long long>(a.digits_[i]) * b.digits_[j];
        }
    }
    long long carry = 0;
    for (size_t i = 0; i < temp.size(); ++i) {
        long long total = temp[i] + carry;
        temp[i] = total % 10;
        carry = total / 10;
    }
    while (carry > 0) {
        temp.push_back(carry % 10);
        carry /= 10;
    }
    // Удаляем ведущие нули (в конце вектора)
    while (temp.size() > 1 && temp.back() == 0) {
        temp.pop_back();
    }
    std::vector<int> digits(temp.size());
    for (size_t i = 0; i < temp.size(); ++i) {
        digits[i] = static_cast<int>(temp[i]);
    }
    BigInteger result;
    result.digits_ = std::move(digits);
    result.negative_ = false;
    return result;
}

std::pair<BigInteger, BigInteger> BigInteger::divide_abs(const BigInteger& dividend,
                                                         const BigInteger& divisor) {
    if (compare_abs(dividend, divisor) < 0) {
        return {BigInteger(0), dividend};
    }
    std::vector<int> div_digits = dividend.digits_;
    std::reverse(div_digits.begin(), div_digits.end());
    BigInteger current;
    current.digits_ = {0};
    current.negative_ = false;
    std::vector<int> quotient_digits;
    for (size_t i = 0; i < div_digits.size(); ++i) {
        current = current * BigInteger(10);
        current = current + BigInteger(div_digits[i]);
        int q = 0;
        for (int d = 9; d >= 0; --d) {
            BigInteger test = divisor * BigInteger(d);
            if (compare_abs(test, current) <= 0) {
                q = d;
                break;
            }
        }
        current = current - (divisor * BigInteger(q));
        quotient_digits.push_back(q);
    }
    std::reverse(quotient_digits.begin(), quotient_digits.end());
    while (quotient_digits.size() > 1 && quotient_digits.back() == 0) {
        quotient_digits.pop_back();
    }
    BigInteger quotient;
    quotient.digits_ = quotient_digits;
    quotient.negative_ = false;
    quotient.normalize();
    current.normalize();
    return {quotient, current};
}

BigInteger::BigInteger() : digits_{0}, negative_(false) {}

BigInteger::BigInteger(int value) {
    *this = BigInteger(std::to_string(value));
}

BigInteger::BigInteger(long long value) {
    *this = BigInteger(std::to_string(value));
}

BigInteger::BigInteger(const std::string& str) {
    size_t pos = 0;
    bool neg = false;
    if (str[0] == '-') {
        neg = true;
        ++pos;
    }
    while (pos < str.size() && str[pos] == '0') {
        ++pos;
    }
    if (pos == str.size()) {
        digits_ = {0};
        negative_ = false;
        return;
    }
    for (size_t i = pos; i < str.size(); ++i) {
        digits_.push_back(str[i] - '0');
    }
    std::reverse(digits_.begin(), digits_.end());
    negative_ = neg;
    normalize();
}

std::string BigInteger::to_string() const {
    if (is_zero()) return "0";
    std::string result;
    if (negative_) result += '-';
    for (auto it = digits_.rbegin(); it != digits_.rend(); ++it) {
        result += static_cast<char>('0' + *it);
    }
    return result;
}

bool BigInteger::is_zero() const {
    return digits_.size() == 1 && digits_[0] == 0;
}

bool BigInteger::is_negative() const {
    return negative_ && !is_zero();
}

BigInteger::operator bool() const {
    return !is_zero();
}

bool BigInteger::operator==(const BigInteger& rhs) const {
    return negative_ == rhs.negative_ && digits_ == rhs.digits_;
}

bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}

bool BigInteger::operator<(const BigInteger& rhs) const {
    if (negative_ != rhs.negative_) {
        return negative_;
    }
    int cmp = compare_abs(*this, rhs);
    if (negative_) {
        return cmp > 0;
    } else {
        return cmp < 0;
    }
}

bool BigInteger::operator>(const BigInteger& rhs) const {
    return rhs < *this;
}

bool BigInteger::operator<=(const BigInteger& rhs) const {
    return !(rhs < *this);
}

bool BigInteger::operator>=(const BigInteger& rhs) const {
    return !(*this < rhs);
}

BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    if (negative_ == rhs.negative_) {
        BigInteger res = add_abs(*this, rhs);
        res.negative_ = negative_;
        return res;
    } else {
        int cmp = compare_abs(*this, rhs);
        if (cmp == 0) {
            return BigInteger(0);
        } else if (cmp > 0) {
            BigInteger res = subtract_abs(*this, rhs);
            res.negative_ = negative_;
            return res;
        } else {
            BigInteger res = subtract_abs(rhs, *this);
            res.negative_ = rhs.negative_;
            return res;
        }
    }
}

BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    return *this + (-rhs);
}

BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    BigInteger res = multiply_abs(*this, rhs);
    res.negative_ = (negative_ != rhs.negative_);
    if (res.is_zero()) res.negative_ = false;
    return res;
}

BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    auto p = divide_abs(this->abs(), rhs.abs());
    BigInteger q = p.first;
    q.negative_ = (negative_ != rhs.negative_);
    if (q.is_zero()) q.negative_ = false;
    return q;
}

BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    auto p = divide_abs(this->abs(), rhs.abs());
    BigInteger r = p.second;
    r.negative_ = negative_;
    if (r.is_zero()) r.negative_ = false;
    return r;
}

BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    *this = *this + rhs;
    return *this;
}

BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    *this = *this - rhs;
    return *this;
}

BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    *this = *this * rhs;
    return *this;
}

BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
    *this = *this / rhs;
    return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    *this = *this % rhs;
    return *this;
}

BigInteger BigInteger::operator-() const {
    BigInteger res = *this;
    if (!res.is_zero()) {
        res.negative_ = !res.negative_;
    }
    return res;
}

BigInteger& BigInteger::operator++() {
    *this += BigInteger(1);
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    ++(*this);
    return old;
}

BigInteger& BigInteger::operator--() {
    *this -= BigInteger(1);
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    --(*this);
    return old;
}

BigInteger BigInteger::abs() const {
    BigInteger res = *this;
    res.negative_ = false;
    return res;
}

void BigInteger::absSum(const BigInteger& other) {
    BigInteger a = this->abs();
    BigInteger b = other.abs();
    *this = a + b;
}

void BigInteger::absSub(const BigInteger& other) {
    BigInteger a = this->abs();
    BigInteger b = other.abs();
    *this = a - b;
}

void BigInteger::absMul(const BigInteger& other) {
    BigInteger a = this->abs();
    BigInteger b = other.abs();
    *this = a * b;
}

void BigInteger::absDiv(const BigInteger& divisor) {
    BigInteger a = this->abs();
    BigInteger b = divisor.abs();
    *this = a / b;
}

std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    os << value.to_string();
    return os;
}

std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string s;
    is >> s;
    value = BigInteger(s);
    return is;
}
