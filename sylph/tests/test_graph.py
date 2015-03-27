from testtools import TestCase
from testtools.matchers import Equals

from .. import graph


class ExpandEdgesTests(TestCase):

    def test_empty(self):
        self.assertThat({}, Equals(graph.expand_edges({})))

    def test_one_edge(self):
        self.assertThat({1: [2]}, Equals(graph.expand_edges({1: [2]})))

    def test_two_edge(self):
        self.assertThat({1: [2, 3], 2: [3]}, Equals(graph.expand_edges({1: [2], 2: [3]})))

    def test_circular(self):
        self.assertThat({1: [2, 1], 2: [1, 2]}, Equals(graph.expand_edges({1: [2], 2: [1]})))


class GetDisjointSets(TestCase):

    def test_empty(self):
        self.assertThat([], Equals(graph.get_disjoint_sets({}, [])))

    def test_no_edges(self):
        self.assertThat([[1]], Equals(graph.get_disjoint_sets({}, [1])))

    def test_one_edge(self):
        self.assertThat([[1], [2]], Equals(graph.get_disjoint_sets({2: [1]}, [1, 2])))

    def test_circular(self):
        self.assertThat([[1, 2]], Equals(graph.get_disjoint_sets({2: [1], 1: [2]}, [1, 2])))

    def test_full(self):
        self.assertThat([[2, 1], [4], [3]], Equals(graph.get_disjoint_sets({3: [1], 2: [1], 1: [2]}, [4, 3, 2, 1])))
