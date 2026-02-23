#!/usr/bin/env fp run
//! Runtime exercise for all std::collections modules.

use std::collections::binary_heap::BinaryHeap;
use std::collections::btree_map::BTreeMap;
use std::collections::btree_map::BTreeMapEntry;
use std::collections::btree_set::BTreeSet;
use std::collections::hash_map::HashMap;
use std::collections::hash_map::HashMapEntry;
use std::collections::hash_set::HashSet;
use std::collections::linked_list::LinkedList;
use std::collections::vec_deque::VecDeque;

fn main() {
    println!("=== std::collections runtime support ===");

    let mut btree_map: BTreeMap<i64, i64> = BTreeMap::from([
        BTreeMapEntry { key: 10, value: 100 },
        BTreeMapEntry { key: 20, value: 200 },
    ]);
    btree_map.insert(15, 150);
    println!("btree_map.contains_key(15) = {}", btree_map.contains_key(15));
    println!("btree_map.get_unchecked(20) = {}", btree_map.get_unchecked(20));

    let mut hash_map: HashMap<i64, i64> = HashMap::from([
        HashMapEntry { key: 1, value: 11 },
        HashMapEntry { key: 2, value: 22 },
    ]);
    hash_map.insert(3, 33);
    println!("hash_map.len = {}", hash_map.len());
    println!("hash_map.get_unchecked(3) = {}", hash_map.get_unchecked(3));

    let mut btree_set: BTreeSet<i64> = BTreeSet::from([3, 1, 2]);
    btree_set.insert(4);
    println!("btree_set.contains(4) = {}", btree_set.contains(4));

    let mut hash_set: HashSet<i64> = HashSet::from([7, 8, 9]);
    hash_set.insert(10);
    println!("hash_set.contains(10) = {}", hash_set.contains(10));

    let mut deque: VecDeque<i64> = VecDeque::new();
    deque.push_back(2);
    deque.push_front(1);
    deque.push_back(3);
    println!("deque.front = {}", deque.front_unchecked());
    println!("deque.back = {}", deque.back_unchecked());
    println!("deque.pop_front = {}", deque.pop_front_unchecked());
    println!("deque.pop_back = {}", deque.pop_back_unchecked());

    let mut list: LinkedList<i64> = LinkedList::new();
    list.push_back(5);
    list.push_front(4);
    list.push_back(6);
    println!("list.front = {}", list.front_unchecked());
    println!("list.back = {}", list.back_unchecked());
    println!("list.pop_front = {}", list.pop_front_unchecked());
    println!("list.pop_back = {}", list.pop_back_unchecked());

    let mut heap: BinaryHeap<i64> = BinaryHeap::from([10, 30, 20]);
    heap.push(40);
    println!("heap.peek = {}", heap.peek_unchecked());
    println!("heap.pop = {}", heap.pop_unchecked());
    println!("heap.pop = {}", heap.pop_unchecked());
}
