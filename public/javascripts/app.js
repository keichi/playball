var beerApp = angular.module('BeerApp', []);

beerApp.controller('BeerListCtrl', function ($scope, $http) {
	$http.get('/api/beerbrand/', {
		params: {
			strength_ge: 5.0
		}
	}).success(function(data) {
		$scope.beers = data;
	});
});
