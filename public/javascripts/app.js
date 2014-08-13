var beerApp = angular.module('BeerApp', []);

beerApp.controller('FlashMessageCtrl', function($scope, $http) {
	$scope.showSuccess = true;
	$scope.showInfo = true;
	$scope.showWarning = true;
	$scope.showError = true;
});

beerApp.controller('BeerListCtrl', function ($scope, $http) {
	$http.get('/api/beerbrand/', {
		params: {
			strength_ge: 5.0
		}
	}).success(function(data) {
		$scope.beers = data;
	});
});
