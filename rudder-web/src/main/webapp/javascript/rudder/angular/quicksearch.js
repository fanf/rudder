var quicksearch = angular.module('quicksearch', ["ngTouch", "angucomplete-ie8"]);

quicksearch.controller('QuicksearchCtrl', ['$scope', '$http', '$rootScope', function QuicksearchCtrl($scope, $http, $rootScope) {

  $scope.setContextPath = function(path) {
    $scope.contextPath = path;
  }

  $scope.getQuicksearchUrl = function() {
    return "/rudder-web/secure/api/quicksearch/"
  }

  $scope.remoteUrlRequestFn = function(str) {
    return {q: str};
  };


} ] );


// Helper function to access from outside angular scope

function initQuicksearchUrl(url) {
  var scope = angular.element($("#quicksearch")).scope();
  scope.$apply(function() {
    scope.setContextPath(url);
  });
};

