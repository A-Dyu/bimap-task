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
        node() : left(nullptr), right(nullptr), p(nullptr), value() {}

        template<typename Y>
        explicit node(Y val) : left(nullptr), right(nullptr), p(nullptr), value(std::move(val)) {}

        bool has_value() const noexcept {
            return value.has_value();
        }

        T const& get_value() const noexcept {
            return value.value();
        }

        void set_value(T const& val) {
            value = val;
        };

        node<T, Tag>* left;
        node<T, Tag>* right;
        node<T, Tag>* p;
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
        using ptr_pair = std::pair<node_t*, node_t*>;

        tree(node_t* end, Comp comp) noexcept : comp(comp), head(end), begin(end), end(end) {}

        node_t* find(T const& val) const noexcept {
            node_t* t = head;
            while (t && (is_valuable(t) || t->left || t->right)) {
                if (is_valuable(t) && equal(t->get_value(), val)) {
                    return t;
                } else if (!is_valuable(t) || comp(val, t->get_value())) {
                    t = t->left;
                } else {
                    t = t->right;
                }
            }
            return nullptr;
        }

        void insert(node_t* new_val) noexcept {
            if (!is_valuable(begin) || comp(new_val->get_value(), begin->get_value())) {
                begin = new_val;
            }
            ptr_pair nodes = split<false>(head, new_val->get_value());
            nodes.first = merge(nodes.first, new_val);
            head = merge(nodes.first, nodes.second);
        }

        void erase(node_t* elem) noexcept {
            if (begin == elem) {
                begin = next(begin);
            }
            ptr_pair nodes1 = split<false>(head, elem->get_value());
            ptr_pair nodes2 = split<true>(nodes1.second, elem->get_value());
            head = merge(nodes1.first, nodes2.second);
        }

        template<typename Delete_type>
        void erase_range(node_t* first, node_t* last) noexcept {
            if (begin == first) {
                begin = last;
            }
            ptr_pair nodes1 = split<false>(head, first->get_value());
            if (last != end) {
                ptr_pair nodes2 = split<false>(nodes1.second, last->get_value());
                head = merge(nodes1.first, nodes2.second);
                destroy<Delete_type>(nodes2.first);
            } else {
                clear_parents(end);
                end->left = nullptr;
                if (end->p) {
                    (end->p->left == end ? end->p->left : end->p->right) = nullptr;
                }
                head = merge(nodes1.first, end);
                destroy<Delete_type>(nodes1.second);
            }
        }

        static node_t* prev(node_t* cur) noexcept {
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
            return cur->p;
        }

        static node_t* next(node_t* cur) noexcept {
            assert(cur->has_value());
            if (cur->right) {
                cur = cur->right;
                while (cur->left) {
                    cur = cur->left;
                }
                return cur;
            }
            while (cur->p->left != cur) {
                cur = cur->p;
            }
            return cur->p;
        }

        void swap(tree& other) noexcept {
            std::swap(head, other.head);
            std::swap(begin, other.begin);
            std::swap(end, other.head);
        }

        node_t* lower_bound(T const& val) const noexcept {
            return bound<false>(head, val);
        }

        node_t* upper_bound(T const& val) const noexcept {
            return bound<true>(head, val);
        }

        bool empty() const noexcept {
            return begin == end;
        }

        node_t* get_begin() const noexcept {
            return begin;
        }

        node_t* get_end() const noexcept {
            return end;
        }

        bool equal(T const& a, T const& b) const noexcept {
            return !comp(a, b) && !comp(b, a);
        }

        template<typename Delete_type>
        void destroy() {
            destroy<Delete_type>(head);
        }

        Comp comp;

        bool up_comp(T const& a, T const& b) const {
            return !(comp(b, a));
        }

    private:
        template<bool Is_up_comp>
        node_t* bound(node_t* ptr, T const& val) const noexcept {
            if (!ptr) {
                return nullptr;
            }
            bool comp_res;
            if constexpr (Is_up_comp) {
                comp_res = !is_valuable(ptr) || !up_comp(ptr->get_value(), val);
            } else {
                comp_res = !is_valuable(ptr) || !comp(ptr->get_value(), val);
            }
            if (comp_res) {
                node_t* l_bound = bound<Is_up_comp>(ptr->left, val);
                if (l_bound) {
                    return l_bound;
                } else {
                    return ptr;
                }
            }
            return bound<Is_up_comp>(ptr->right, val);
        }

        template<bool Is_up_comp>
        ptr_pair split(node_t* t, T const& val) noexcept {
            if (!t) {
                return {nullptr, nullptr};
            }
            clear_parents(t);
            bool comp_res;
            if constexpr (Is_up_comp) {
            comp_res = is_valuable(t) && up_comp(t->get_value(), val);
            } else {
            comp_res = is_valuable(t) && comp(t->get_value(), val);
            }
            if (comp_res) {
                ptr_pair res = split<Is_up_comp>(t->right, val);
                t->right = res.first;
                ensure_parents(t);
                return {t, res.second};
            } else {
                ptr_pair res = split<Is_up_comp>(t->left, val);
                t->left = res.second;
                ensure_parents(t);
                return {res.first, t};
            }
        }

        node_t* merge(node_t* l, node_t* r) noexcept {
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

        static bool is_valuable(node_t* t) noexcept {
            return t && t->has_value();
        }

        static void ensure_parents(node_t* t) noexcept {
            if (t->left) {
                t->left->p = t;
            }
            if (t->right) {
                t->right->p = t;
            }
        }

        static void clear_parents(node_t* t) noexcept {
            if (t->left) {
                t->left->p = nullptr;
            }
            if (t->right) {
                t->right->p = nullptr;
            }
        }

        template<typename Delete_type>
        static void destroy(node_t* ptr) {
            if (!ptr) {
                return;
            }
            destroy<Delete_type>(ptr->left);
            destroy<Delete_type>(ptr->right);
            delete static_cast<Delete_type>(ptr);
        }

        node_t* head;
        node_t* begin;
        node_t* end;
    };

    template<typename T, typename Tag, typename Comp>
    struct base_iterator {
        using node_t = node<T, Tag>;
        using tree_t = tree<T, Tag, Comp>;

        base_iterator(node_t* node) noexcept : it_node(node) {}

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

        node_t* it_node;
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

        right_iterator(node<Right, right_tag>* node) noexcept : base(node) {}

        right_iterator& operator++() noexcept {
            base::it_node = tree_t::next(base::it_node);
            return *this;
        }

        right_iterator operator++(int) noexcept {
            auto old = *this;
            ++(*this);
            return old;
        }

        right_iterator& operator--() noexcept {
            base::it_node = tree_t::prev(base::it_node);
            return *this;
        }

        right_iterator operator--(int) noexcept {
            auto old = *this;
            --(*this);
            return old;
        }

        friend bool operator==(right_iterator const& a, right_iterator const& b) {
            return a.base::it_node == b.base::it_node;
        }

        friend bool operator!=(right_iterator const& a, right_iterator const& b) {
            return !(a == b);
        }

        left_iterator flip() const noexcept {
            return static_cast<bi_node*>(base::it_node);
        }
    };


    struct left_iterator : base_iterator<Left, left_tag, CompareLeft> {
        using base = base_iterator<Left, left_tag, CompareLeft>;
        using tree_t = tree<Left, left_tag, CompareLeft>;

        friend struct bimap<Left, Right, CompareLeft, CompareRight>;

        left_iterator(node<Left, left_tag>* node) noexcept : base(node) {}

        left_iterator& operator++() noexcept {
            base::it_node = tree_t::next(base::it_node);
            return *this;
        }

        left_iterator operator++(int) noexcept {
            auto old = *this;
            ++(*this);
            return old;
        }

        left_iterator& operator--() noexcept {
            base::it_node = tree_t::prev(base::it_node);
            return *this;
        }

        left_iterator operator--(int) noexcept {
            auto old = *this;
            --(*this);
            return old;
        }

        friend bool operator==(left_iterator const& a, left_iterator const& b) {
            return a.base::it_node == b.base::it_node;
        }

        friend bool operator!=(left_iterator const& a, left_iterator const& b) {
            return !(a == b);
        }

        right_iterator flip() const noexcept {
            return static_cast<bi_node*>(base::it_node);
        }
    };

    bimap(CompareLeft cmpL = CompareLeft(), CompareRight cmpR = CompareRight()) noexcept : l_tree(new bi_node(), cmpL), r_tree(static_cast<bi_node*>(l_tree.get_end()), cmpR), bimap_size(0) {}

    bimap(bimap const& other): bimap() {
        copy(other);
    }

    bimap(bimap&& other) noexcept : bimap() {
        swap(other);
    }

    ~bimap() {
        l_tree.template destroy<bi_node*>();
    }

    bimap& operator=(bimap const& other) {
        if (this != &other) {
            bimap safe(other);
            swap(safe);
        }
        return *this;
    }

    bimap& operator=(bimap&& other) noexcept {
        if (this != &other) {
            bimap safe(std::move(other));
            swap(safe);
        }
        return *this;
    }

    left_iterator insert(Left const& l_val, Right const& r_val) noexcept {
        if (find_left(l_val) != end_left() || find_right(r_val) != end_right()) {
            return end_left();
        }
        auto* new_elem = new bi_node(l_val, r_val);
        insert(new_elem);
        return new_elem;
    }

    left_iterator insert(Left&& l_val, Right const& r_val) noexcept {
        if (find_left(l_val) != end_left() || find_right(r_val) != end_right()) {
            return end_left();
        }
        auto* new_elem = new bi_node(std::move(l_val), r_val);
        insert(new_elem);
        return new_elem;
    }

    left_iterator insert(Left const& l_val, Right&& r_val) noexcept {
        if (find_left(l_val) != end_left() || find_right(r_val) != end_right()) {
            return end_left();
        }
        auto* new_elem = new bi_node(l_val, std::move(r_val));
        insert(new_elem);
        return new_elem;
    }

    left_iterator insert(Left&& l_val, Right&& r_val) noexcept {
        if (find_left(l_val) != end_left() || find_right(r_val) != end_right()) {
            return end_left();
        }
        auto* new_elem = new bi_node(std::move(l_val), std::move(r_val));
        insert(new_elem);
        return new_elem;
    }

    left_iterator erase_left(left_iterator it) {
        left_iterator res = it;
        res++;
        erase(static_cast<bi_node*>(it.it_node));
        return res;
    }

    bool erase_left(Left const& left) {
        l_node* ptr = l_tree.find(left);
        if (ptr) {
            erase_left(ptr);
        }
        return static_cast<bool>(ptr);
    }

    right_iterator erase_right(right_iterator it) {
        right_iterator res = it;
        res++;
        erase(static_cast<bi_node*>(it.it_node));
        return res;
    }

    bool erase_right(Right const& right) {
        r_node* ptr = r_tree.find(right);
        if (ptr) {
            erase_right(ptr);
        }
        return static_cast<bool>(ptr);
    }

    left_iterator find_left (Left const& left) const noexcept {
        l_node* ptr = l_tree.find(left);
        return ptr ? ptr : end_left();
    }

    right_iterator find_right(Right const& right) const noexcept {
        r_node* ptr = r_tree.find(right);
        return ptr ? ptr : end_right();
    }

    Right const& at_left(Left const& key) const {
        auto* ptr = static_cast<bi_node*>(l_tree.find(key));
        if (!ptr) {
            throw std::out_of_range("No such key in bimap");
        }
        return ptr->r_node::get_value();
    }

    Left const& at_right(Right const& key) const {
        auto* ptr = static_cast<bi_node*>(r_tree.find(key));
        if (!ptr) {
            throw std::out_of_range("No such key in bimap");
        }
        return ptr->l_node::get_value();
    }

    template<typename U = Right, typename = std::enable_if_t<std::is_default_constructible_v<U>>>
    Right const& at_left_or_default(Left const& key) noexcept {
        auto* ptr = static_cast<bi_node*>(l_tree.find(key));
        if (ptr) {
            return ptr->r_node::get_value();
        } else {
            Right def = Right();
            r_node* pd = r_tree.find(def);
            if (pd) {
                erase(static_cast<bi_node*>(pd));
            }
            return *insert(key, std::move(def)).flip();
        }
    }

    template<typename U = Left, typename = std::enable_if_t<std::is_default_constructible_v<U>>>
    Left const& at_right_or_default(Right const& key) noexcept {
        auto* ptr = static_cast<bi_node*>(r_tree.find(key));
        if (ptr) {
            return ptr->l_node::get_value();
        } else {
            Left def = Left();
            l_node* pd = l_tree.find(def);
            if (pd) {
                erase(static_cast<bi_node*>(pd));
            }
            return *insert(std::move(def), key);
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

    size_t size() const noexcept {
        return bimap_size;
    }

    friend bool operator==(bimap const& a, bimap const& b) noexcept {
        if (a.size() != b.size()) {
            return false;
        }
        for (auto it1 = a.begin_left(), it2 = b.begin_left(); it1 != a.end_left(); it1++, it2++) {
            if (!a.l_tree.equal(*it1, *it2) || !a.r_tree.equal(*it1.flip(), *it2.flip())) {
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
            r_node* ptr = static_cast<bi_node*>(it.it_node);
            it++;
            r_tree.erase(ptr);
        }
        l_tree.template erase_range<bi_node*>(first.it_node, last.it_node);
        return last;
    }

    right_iterator erase_right(right_iterator first, right_iterator last) {
        for (auto it = first; it != last;) {
            bimap_size--;
            l_node* ptr = static_cast<bi_node*>(it.it_node);
            it++;
            l_tree.erase(ptr);
        }
        r_tree.template erase_range<bi_node*>(first.it_node, last.it_node);
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


    void copy(bimap const& other) {
        for (auto it = other.begin_left(); it != other.end_left(); it++) {
            auto* ptr = static_cast<bi_node*>(it.it_node);
            insert(ptr->l_node::get_value(), ptr->r_node::get_value());
        }
    }

    void erase(bi_node* ptr) noexcept {
        bimap_size--;
        l_tree.erase(ptr);
        r_tree.erase(ptr);
        delete ptr;
    }

    void insert(bi_node* const& new_elem) noexcept {
        bimap_size++;
        l_tree.insert(new_elem);
        r_tree.insert(new_elem);
    }

    tree<Left, left_tag, CompareLeft> l_tree;
    tree<Right, right_tag, CompareRight> r_tree;
    std::size_t bimap_size;
};
