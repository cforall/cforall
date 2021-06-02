//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Heap.h --
//
// Author           : Thierry Delisle
// Created On       : Fri Mar 03 14:53:53 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once

#include <cstdint>
#include <iostream>

namespace Stats {
	namespace Base {
		class TreeImpl;

		struct TreeTop {
			TreeImpl * head = nullptr;
			TreeImpl * tail = nullptr;

			inline void append(TreeImpl * node);
		};

		template<typename func_t>
		void ForAll(TreeTop & range, std::size_t level, func_t func, bool destroy = false);

		class TreeImpl {
		public:
			virtual void print(std::ostream &) = 0;

			const char * const name;
			TreeImpl(const char * const name) : name(name) {}

		protected:
			virtual ~TreeImpl() = default;

			TreeImpl * next = nullptr;
			TreeTop children;

			friend struct TreeTop;

			template<typename func_t>
			friend void ForAll(TreeTop & range, std::size_t level, func_t func, bool destroy);
		};

		void TreeTop::append(TreeImpl * node) {
			if(!head) { head = node; }
			else      { tail->next = node;}
			tail = node;
		}

		template<typename func_t>
		inline void ForAll(TreeTop & range, std::size_t level, func_t func, bool destroy) {
			auto it = range.head;
			while(it) {
				auto next = it->next;
				func(it, level);
				ForAll(it->children, level + 1, func);
				if(destroy) delete it;
				it = next;
			}
		}

		template<TreeTop & top>
		class Tree : public TreeImpl {
		public:
			Tree(const char * const name) : TreeImpl{name} {
				top.append(this);
			}

			Tree(const char * const name, Tree * parent) : TreeImpl{name} {
				parent->children.append(this);
			}
		protected:
			virtual ~Tree() = default;
		};
	}
}