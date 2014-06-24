var beerApp = angular.module('BeerApp', []);

beerApp.controller('BeerListCtrl', function ($scope, $http) {
	$http.get('/api/beerbrand/').success(function(data) {
		$scope.beers = data;
	});
});
