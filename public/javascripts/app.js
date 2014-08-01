var beerApp = angular.module('BeerApp', []);

beerApp.controller('BeerListCtrl', function ($scope, $http) {
	$http.get('/api/beerbrand/', {
		params: {
			strength_ge: 5.0
		}
	}).success(function(data) {
		data.forEach(function(item) {
			item.createdAt = new Date(item.createdAt)
			item.updatedAt = new Date(item.updatedAt)
		})

		$scope.beers = data;
	});
});
