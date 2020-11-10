#pragma once
#include <cassert>
#include <functional>
#include <memory>
#include <optional>
#include <random>

namespace {
    struct left_tag;
    struct right_tag;

    struct priority {
        priority() noexcept : x(rnd()) {}

        uint32_t get_priority() const noexcept {
            return x;
        }

    private:
        static inline auto rnd = std::mt19937(1488322);
        uint32_t x;
    };

    template<typename T, typename Tag>
    struct node : virtual priority {
        node() = default;

        template<typename Y>
        explicit node(Y val) : value(std::move(val)) {}

        bool has_value() const noexcept {
            return value.has_value();
        }

        T const& get_value() const noexcept {
            return value.value();
        }

        std::shared_ptr<node<T, Tag>> left;
        std::shared_ptr<node<T, Tag>> right;
        std::weak_ptr<node<T, Tag>> p;
    private:
        std::optional<T> value;
    };

    template<typename Left, typename Right>
    struct binode : node<Left, left_tag>, node<Right, right_tag> {
        binode() = default;

        template<typename L, typename R>
        binode(L l_val, R r_val) : node<Left, left_tag>(std::move(l_val)), node<Right, right_tag>(std::move(r_val)) {}
    };

    template<typename T, typename Tag, typename Comp>
    struct tree {
        using node_t = node<T, Tag>;
        using s_ptr = std::shared_ptr<node_t>;
        using w_ptr = std::weak_ptr<node_t>;
        using s_pair = std::pair<s_ptr, s_ptr>;

        tree(s_ptr end) noexcept : head(end), begin(end), end(end) {}

        s_ptr find(T const& val) const noexcept {
            s_ptr t = head;
            while (t && (is_valuable(t) || t->left || t->right)) {
                if (is_valuable(t) && t->get_value() == val) {
                    return t;
                } else if (!is_valuable(t) || comp(val, t->get_value())) {
                    t = t->left;
                } else {
                    t = t->right;
                }
            }
            return nullptr;
        }

        void insert(s_ptr new_val) noexcept {
            if (!is_valuable(begin) || comp(new_val->get_value(), begin->get_value())) {
                begin = new_val;
            }
            s_pair nodes = split(head, new_val->get_value(), comp);
            nodes.first = merge(nodes.first, new_val);
            head = merge(nodes.first, nodes.second);
        }

        void erase(s_ptr elem) noexcept {
            if (begin == elem) {
                begin = next(begin);
            }
            s_pair nodes1 = split(head, elem->get_value(), comp);
            s_pair nodes2 = split(nodes1.second, elem->get_value(), up_comp);
            head = merge(nodes1.first, nodes2.second);
        }

        void erase_range(s_ptr first, s_ptr last) noexcept {
            if (begin == first) {
                begin = last;
            }
            s_pair nodes1 = split(head, first->get_value(), comp);
            if (last != end) {
                s_pair nodes2 = split(nodes1.second, last->get_value(), comp);
                head = merge(nodes1.first, nodes2.second);
            } else {
                clear_parents(end);
                end->left = nullptr;
                head = merge(nodes1.first, end);
            }
        }

        static s_ptr prev(s_ptr cur) noexcept {
            if (cur->left) {
                cur = cur->left;
                while (cur->right) {
                    cur = cur->right;
                }
                return cur;
            }
            while(cur->p->right != cur) {
                cur = cur->p;
                assert(cur->p);
            }
            return cur->p.lock();
        }

        static s_ptr next(s_ptr cur) noexcept {
            assert(cur->has_value());
            if (cur->right) {
                cur = cur->right;
                while (cur->left) {
                    cur = cur->left;
                }
                return cur;
            }
            while (cur->p.lock()->left != cur) {
                cur = cur->p.lock();
            }
            return cur->p.lock();
        }

        void swap(tree& other) noexcept {
            std::swap(head, other.head);
            std::swap(end, other.head);
        }

        s_ptr lower_bound(T const& val) const noexcept {
            return bound(head, val, comp);
        }

        s_ptr upper_bound(T const& val) const noexcept {
            return bound(head, val, up_comp);
        }

        bool empty() const noexcept {
            return begin == end;
        }

        s_ptr get_begin() const noexcept {
            return begin;
        }

        s_ptr get_end() const noexcept {
            return end;
        }

        template<typename L, typename R, typename CL, typename CR>
        friend struct bimap;

    private:
        static constexpr Comp comp = Comp();

        static constexpr auto up_comp = [](T const& a, T const& b) {
            return comp(a, b) || a == b;
        };

        template<typename F>
        static s_ptr bound(s_ptr ptr, T const& val, F const& bound_comp) noexcept {
            if (!ptr) {
                return nullptr;
            }
            if (!is_valuable(ptr) || !bound_comp(ptr->get_value(), val)) {
                s_ptr l_bound = bound(ptr->left, val, bound_comp);
                if (l_bound) {
                    return l_bound;
                } else {
                    return ptr;
                }
            }
            return bound(ptr->right, val, bound_comp);
        }

        template<typename F>
        static s_pair split(s_ptr t, T const& val, F const& split_comp) noexcept {
            if (!t) {
                return {nullptr, nullptr};
            }
            clear_parents(t);
            if (is_valuable(t) && split_comp(t->get_value(), val)) {
                s_pair res = split(t->right, val, split_comp);
                t->right = res.first;
                ensure_parents(t);
                return {t, res.second};
            } else {
                s_pair res = split(t->left, val, split_comp);
                t->left = res.second;
                ensure_parents(t);
                return {res.first, t};
            }
        }

        static s_ptr merge(s_ptr l, s_ptr r) noexcept {
            if (!l) {
                return r;
            }
            if (!r) {
                return l;
            }
            if (l->get_priority() < r->get_priority()) {
                l->right = merge(l->right, r);
                ensure_parents(l);
                return l;
            } else {
                r->left = merge(l, r->left);
                ensure_parents(r);
                return r;
            }
        }

        static bool is_valuable(s_ptr t) noexcept {
            return t && t->has_value();
        }

        static void ensure_parents(s_ptr t) noexcept {
            if (t->left) {
                t->left->p = t;
            }
            if (t->right) {
                t->right->p = t;
            }
        }

        static void clear_parents(s_ptr t) noexcept {
            if (t->left) {
                t->left->p = w_ptr();
            }
            if (t->right) {
                t->right->p = w_ptr();
            }
        }

        s_ptr head;
        s_ptr begin;
        s_ptr end;
    };

    template<typename T, typename Tag, typename Comp>
    struct base_iterator {
        using node_t = node<T, Tag>;
        using ptr = std::shared_ptr<node_t>;
        using tree_t = tree<T, Tag, Comp>;

        base_iterator(ptr node) noexcept : it_node(node) {}

        T const& operator*() const noexcept {
            return it_node->get_value();
        }

    protected:
        template<typename Iterator, typename Ptr>
        static Iterator prefix_transform(Iterator& cur, std::function<Ptr (Ptr)> transformer) noexcept {
            Iterator old(cur);
            cur.it_node = transformer(cur.it_node);
            return old;
        }

        template<typename Iterator, typename Ptr>
        static Iterator& postfix_transform(Iterator& cur, std::function<Ptr (Ptr)> transformer) noexcept {
            cur.it_node = transformer(cur.it_node);
            return cur;
        }

        ptr it_node;
    };
}

template <typename Left, typename Right,
        typename CompareLeft = std::less<Left>,
        typename CompareRight = std::less<Right>>
struct bimap {
    using left_t = Left;
    using right_t = Right;

    struct left_iterator;

    struct right_iterator : base_iterator<Right, right_tag, CompareRight> {
        using base = base_iterator<Right, right_tag, CompareRight>;
        using tree_t = tree<Right, right_tag, CompareRight>;

        friend struct bimap<Left, Right, CompareLeft, CompareRight>;

        right_iterator(std::shared_ptr<node<Right, right_tag>> node) noexcept : base(node) {}

        right_iterator& operator++() noexcept {
            return base::template postfix_transform<right_iterator, r_ptr>(*this, tree_t::next);
        }

        right_iterator operator++(int) noexcept {
            return base::template prefix_transform<right_iterator, r_ptr>(*this, tree_t::next);
        }

        right_iterator& operator--() noexcept {
            return base::template postfix_transform<right_iterator, r_ptr>(*this, tree_t::prev);
        }

        right_iterator operator--(int) noexcept {
            return base::template prefix_transform<right_iterator, r_ptr>(*this, tree_t::prev);
        }

        friend bool operator==(right_iterator const& a, right_iterator const& b) {
            return a.base::it_node == b.base::it_node;
        }

        friend bool operator!=(right_iterator const& a, right_iterator const& b) {
            return !(a == b);
        }

        left_iterator flip() const noexcept {
            return bimap::flip(base::it_node);
        }
    };


    struct left_iterator : base_iterator<Left, left_tag, CompareLeft> {
        using base = base_iterator<Left, left_tag, CompareLeft>;
        using tree_t = tree<Left, left_tag, CompareLeft>;

        friend struct bimap<Left, Right, CompareLeft, CompareRight>;

        left_iterator(std::shared_ptr<node<Left, left_tag>> node) noexcept : base(node) {}

        left_iterator& operator++() noexcept {
            return base::template postfix_transform<left_iterator, l_ptr>(*this, tree_t::next);
        }

        left_iterator operator++(int) noexcept {
            return base::template prefix_transform<left_iterator, l_ptr>(*this, tree_t::next);
        }

        left_iterator& operator--() noexcept {
            return base::template postfix_transform<left_iterator, l_ptr>(*this, tree_t::prev);
        }

        left_iterator operator--(int) noexcept {
            return base::template prefix_transform<left_iterator, l_ptr>(*this, tree_t::prev);
        }

        friend bool operator==(left_iterator const& a, left_iterator const& b) {
            return a.base::it_node == b.base::it_node;
        }

        friend bool operator!=(left_iterator const& a, left_iterator const& b) {
            return !(a == b);
        }

        right_iterator flip() const noexcept {
            return bimap::flip(base::it_node);
        }
    };

    bimap() noexcept : l_tree(to_l_ptr(std::make_shared<bi_node>())), r_tree(flip(l_tree.get_end())), bimap_size(0) {}

    bimap(bimap const& other): bimap() {
        copy(other);
    }

    bimap(bimap&& other) noexcept : bimap() {
        swap(other);
    }

    bimap& operator=(bimap const& other) {
        if (this != &other) {
            this->~bimap();
            new(this) bimap(other);
        }
        return *this;
    }

    bimap& operator=(bimap&& other) noexcept {
        if (this != &other) {
            this->~bimap();
            new(this) bimap(std::move(other));
        }
        return *this;
    }

    template<typename L, typename R,
            typename = std::enable_if<std::is_same_v<std::decay_t<L>, Left>>,
            typename = std::enable_if<std::is_same_v<std::decay_t<R>, Right>>>
    left_iterator insert(L l_val, R r_val) noexcept {
        if (find_left(l_val) != end_left() || find_right(r_val) != end_right()) {
            return end_left();
        }
        bi_ptr new_elem = std::make_shared<bi_node>(std::move(l_val), std::move(r_val));
        insert(new_elem);
        return to_l_ptr(new_elem);
    }

    left_iterator erase_left(left_iterator it) {
        left_iterator res = it;
        res++;
        erase(to_n_ptr(it.it_node));
        return res;
    }

    bool erase_left(Left const& left) {
        l_ptr ptr = l_tree.find(left);
        if (ptr) {
            erase_left(ptr);
        }
        return static_cast<bool>(ptr);
    }

    right_iterator erase_right(right_iterator it) {
        right_iterator res = it;
        res++;
        erase(to_n_ptr(it.it_node));
        return res;
    }

    bool erase_right(Right const& right) {
        r_ptr ptr = r_tree.find(right);
        if (ptr) {
            erase_right(ptr);
        }
        return static_cast<bool>(ptr);
    }

    left_iterator find_left (Left const& left) const noexcept {
        l_ptr ptr = l_tree.find(left);
        return ptr ? ptr : end_left();
    }

    right_iterator find_right(Right const& right) const noexcept {
        r_ptr ptr = r_tree.find(right);
        return ptr ? ptr : end_right();
    }

    Right const& at_left(Left const& key) const {
        bi_ptr ptr = to_n_ptr(l_tree.find(key));
        if (!ptr) {
            throw std::out_of_range("No such key in bimap");
        }
        return ptr->r_node::get_value();
    }

    Left const& at_right(Right const& key) const {
        bi_ptr ptr = to_n_ptr(r_tree.find(key));
        if (!ptr) {
            throw std::out_of_range("No such key in bimap");
        }
        return ptr->l_node::get_value();
    }

    Right at_left_or_default(Left const& key) const noexcept {
        bi_ptr ptr = to_n_ptr(l_tree.find(key));
        if (ptr) {
            return ptr->r_node::get_value();
        } else if constexpr (std::is_default_constructible_v<Right>) {
            return Right();
        } else {
            throw std::runtime_error("No default value");
        }
    }

    Left at_right_or_default(Right const& key) const noexcept {
        bi_ptr ptr = to_n_ptr(r_tree.find(key));
        if (ptr) {
            return ptr->l_node::get_value();
        } else if constexpr (std::is_default_constructible_v<Left>) {
            return Left();
        } else {
            throw std::runtime_error("No default value");
        }
    }

    left_iterator lower_bound_left(const Left& left) const noexcept {
        return l_tree.lower_bound(left);
    }

    left_iterator upper_bound_left(const Left& left) const noexcept {
        return l_tree.upper_bound(left);
    }

    right_iterator lower_bound_right(const Right& right) const noexcept {
        return r_tree.lower_bound(right);
    }

    right_iterator upper_bound_right(const Right& right) const noexcept {
        return r_tree.upper_bound(right);
    }

    left_iterator begin_left() const noexcept {
        return l_tree.get_begin();
    }

    left_iterator end_left() const noexcept {
        return l_tree.get_end();
    }

    right_iterator begin_right() const noexcept {
        return r_tree.get_begin();
    }

    right_iterator end_right() const noexcept {
        return r_tree.get_end();
    }

    bool empty() const noexcept {
        return l_tree.empty();
    }

    std::size_t size() const noexcept {
        return bimap_size;
    }

    friend bool operator==(bimap const& a, bimap const& b) noexcept {
        if (a.size() != b.size()) {
            return false;
        }
        for (auto it1 = a.begin_left(), it2 = b.begin_left(); it1 != a.end_left(); it1++, it2++) {
            if (*it1 != *it2 || *it1.flip() != *it2.flip()) {
                return false;
            }
        }
        return true;
    }

    friend bool operator!=(bimap const& a, bimap const& b) noexcept {
        return !(a == b);
    }

    left_iterator erase_left(left_iterator first, left_iterator last) {
        for (auto it = first; it != last;) {
            bimap_size--;
            auto ptr = flip(it.it_node);
            it++;
            r_tree.erase(ptr);
        }
        l_tree.erase_range(first.it_node, last.it_node);
        return last;
    }

    right_iterator erase_right(right_iterator first, right_iterator last) {
        for (auto it = first; it != last;) {
            bimap_size--;
            auto ptr = flip(it.it_node);
            it++;
            l_tree.erase(ptr);
        }
        r_tree.erase_range(first.it_node, last.it_node);
        return last;
    }

    void swap(bimap& other) noexcept {
        l_tree.swap(other.l_tree);
        r_tree.swap(other.r_tree);
        std::swap(bimap_size, other.bimap_size);
    }

private:
    using l_node = node<Left, left_tag>;
    using r_node = node<Right, right_tag>;
    using bi_node = binode<Left, Right>;

    using l_ptr = std::shared_ptr<l_node>;
    using r_ptr = std::shared_ptr<r_node>;
    using bi_ptr = std::shared_ptr<bi_node>;

    void copy(bimap const& other) {
        for (auto it = other.begin_left(); it != other.end_left(); it++) {
            bi_ptr ptr = to_n_ptr(it.it_node);
            insert(ptr->l_node::get_value(), ptr->r_node::get_value());
        }
    }

    void erase(bi_ptr ptr) noexcept {
        bimap_size--;
        l_tree.erase(to_l_ptr(ptr));
        r_tree.erase(to_r_ptr(ptr));
    }

    void insert(bi_ptr const& new_elem) noexcept {
        bimap_size++;
        l_tree.insert(to_l_ptr(new_elem));
        r_tree.insert(to_r_ptr(new_elem));
    }

    static l_ptr to_l_ptr(bi_ptr const& ptr) noexcept {
        return ptr ? l_ptr(ptr, static_cast<l_node*>(&(*ptr))) : nullptr;
    }

    static r_ptr to_r_ptr(bi_ptr const& ptr) noexcept {
        return ptr ? r_ptr(ptr, static_cast<r_node*>(&(*ptr))) : nullptr;
    }

    static bi_ptr to_n_ptr(l_ptr ptr) noexcept {
        return ptr ? bi_ptr(ptr, static_cast<bi_node*>(&(*ptr))) : nullptr;
    }

    static bi_ptr to_n_ptr(r_ptr ptr) noexcept {
        return ptr ? bi_ptr(ptr, static_cast<bi_node*>(&(*ptr))) : nullptr;
    }

    static l_ptr flip(r_ptr const& ptr) noexcept {
        return to_l_ptr(to_n_ptr(ptr));
    }

    static r_ptr flip(l_ptr const& ptr) noexcept {
        return to_r_ptr(to_n_ptr(ptr));
    }

    tree<Left, left_tag, CompareLeft> l_tree;
    tree<Right, right_tag, CompareRight> r_tree;
    std::size_t bimap_size;
};
