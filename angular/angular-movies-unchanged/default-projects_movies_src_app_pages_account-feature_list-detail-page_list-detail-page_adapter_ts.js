"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["default-projects_movies_src_app_pages_account-feature_list-detail-page_list-detail-page_adapter_ts"],{

/***/ 5172:
/*!****************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/list.resource.ts ***!
  \****************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ListResource: () => (/* binding */ ListResource)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common/http */ 4860);





const URL_LIST_BASE = [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__.baseUrlApiV4, 'list'].join('/');
const URL_EXISTING_LIST = id => [URL_LIST_BASE, id].join('/');
const URL_ADD_MOVIE_TO_LIST = id => [URL_EXISTING_LIST(id), 'items'].join('/');
class ListResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_2__.HttpClient);
    this.createList = params => this.http.post(URL_LIST_BASE, params).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.map)(({
      id
    }) => id));
    this.fetchList = id => this.http.get(URL_EXISTING_LIST(+id)).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.map)(list => ({
      [id]: list
    })));
    this.updateList = params => this.http.put(URL_EXISTING_LIST(params.id || 0), params);
    this.addMovieToList = params => this.http.post(URL_ADD_MOVIE_TO_LIST(params.id), params);
    this.deleteMovieFromList = params => this.http.delete(URL_ADD_MOVIE_TO_LIST(params.id), {
      body: params
    });
    this.deleteList = id => this.http.delete(URL_EXISTING_LIST(+id));
  }
  static #_ = this.ɵfac = function ListResource_Factory(t) {
    return new (t || ListResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineInjectable"]({
    token: ListResource,
    factory: ListResource.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 2951:
/*!****************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/list-detail-page/list-detail-page.adapter.ts ***!
  \****************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ListDetailAdapter: () => (/* binding */ ListDetailAdapter),
/* harmony export */   transformToCastList: () => (/* binding */ transformToCastList),
/* harmony export */   transformToMovieDetail: () => (/* binding */ transformToMovieDetail),
/* harmony export */   transformToMovieModel: () => (/* binding */ transformToMovieModel)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! @rx-angular/state/selections */ 8748);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! rxjs */ 8989);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! rxjs */ 1891);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! rxjs */ 5043);
/* harmony import */ var _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../data-access/images/image-sizes */ 4082);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _state_list_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../../state/list.state */ 7219);
/* harmony import */ var _shared_router_router_state__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../../shared/router/router.state */ 8202);
/* harmony import */ var _shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../../shared/cdk/image/image-tag.transform */ 9699);
/* harmony import */ var _shared_cdk_video_video_tag_transform__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../../../shared/cdk/video/video-tag.transform */ 9705);
/* harmony import */ var _shared_cdk_link_a_tag_transform__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ../../../shared/cdk/link/a-tag.transform */ 746);
/* harmony import */ var _constants__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ../../../constants */ 4765);













class ListDetailAdapter extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_7__.RxState {
  constructor() {
    super();
    this.listState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_8__.inject)(_state_list_state__WEBPACK_IMPORTED_MODULE_1__.ListState);
    this.routerState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_8__.inject)(_shared_router_router_state__WEBPACK_IMPORTED_MODULE_2__.RouterState);
    this.ui = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_9__.RxActionFactory().create();
    this.srcset = '154w, 185w, 342w, 500w, 780w';
    this.routerListId$ = this.routerState.select((0,rxjs__WEBPACK_IMPORTED_MODULE_10__.map)(state => state?.type));
    this.listInfoUpdateEvent$ = this.ui.listInfoUpdate$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_11__.withLatestFrom)(this.select('id')));
    this.listPosterUpdateEvent$ = this.ui.listPosterUpdate$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_11__.withLatestFrom)(this.select('id')));
    this.listDeleteEvent$ = this.ui.deleteList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_11__.withLatestFrom)(this.select('id')));
    this.listDetails$ = this.select('id').pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_12__.switchMap)(id => this.listState.select('lists', id)));
    this.movies$ = this.listDetails$.pipe((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_13__.select)('results'), (0,rxjs__WEBPACK_IMPORTED_MODULE_10__.map)(r => r !== undefined ? r.map(transformToMovieModel) : []));
    this.posters$ = this.listDetails$.pipe((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_13__.selectSlice)(['results', 'backdrop_path']), (0,rxjs__WEBPACK_IMPORTED_MODULE_10__.map)(({
      results,
      backdrop_path
    }) => results?.map(m => ({
      ...(0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__.addImageTag)(m, {
        pathProp: 'backdrop_path',
        dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__.W500H282,
        fallback: _constants__WEBPACK_IMPORTED_MODULE_6__.MY_LIST_FALLBACK
      }),
      selected: m.backdrop_path === backdrop_path
    }))));
    this.listName$ = this.listDetails$.pipe((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_13__.select)('name'), (0,rxjs__WEBPACK_IMPORTED_MODULE_14__.startWith)('Loading...'));
    this.connect('id', this.routerListId$);
    this.hold(this.routerListId$, this.listState.fetchList);
    this.hold(this.listInfoUpdateEvent$, ([info, id]) => this.listState.updateList({
      ...info,
      id: +id
    }));
    this.hold(this.listPosterUpdateEvent$, ([backdrop_path, id]) => this.listState.updateList({
      backdrop_path,
      id: +id
    }));
    this.hold(this.listDeleteEvent$, ([, id]) => this.listState.deleteList(id));
  }
  static #_ = this.ɵfac = function ListDetailAdapter_Factory(t) {
    return new (t || ListDetailAdapter)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_8__["ɵɵdefineInjectable"]({
    token: ListDetailAdapter,
    factory: ListDetailAdapter.ɵfac,
    providedIn: 'root'
  });
}

function transformToMovieDetail(_res) {
  const res = _res;
  let language = false;
  if (Array.isArray(res?.spoken_languages) && res?.spoken_languages.length !== 0) {
    language = res.spoken_languages[0].english_name;
  }
  res.languages_runtime_release = `${language + ' / ' || 0} ${res.runtime} MIN. / ${new Date(res.release_date).getFullYear()}`;
  (0,_shared_cdk_video_video_tag_transform__WEBPACK_IMPORTED_MODULE_4__.addVideoTag)(res, {
    pathPropFn: r => r?.videos?.results && r?.videos?.results[0]?.key + '' || ''
  });
  (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__.addImageTag)(res, {
    pathProp: 'poster_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__.W300H450,
    sizes: `(min-width: 900px) 400px, 65vw`,
    srcset: '154w, 185w, 342w, 500w, 780w'
  });
  (0,_shared_cdk_link_a_tag_transform__WEBPACK_IMPORTED_MODULE_5__.addLinkTag)(res, 'imdb_id', {});
  return res;
}
function transformToCastList(_res) {
  const res = _res;
  (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__.addImageTag)(res, {
    pathProp: 'profile_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__.W92H138,
    sizes: `44px`,
    srcset: '154w'
  });
  return res;
}
function transformToMovieModel(_res) {
  return (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__.addImageTag)(_res, {
    pathProp: 'poster_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__.W154H205,
    sizes: '(min-width: 900px) 20vw, 70vw',
    srcset: '154w, 185w, 342w, 500w, 780w'
  });
}

/***/ }),

/***/ 9699:
/*!*************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/image/image-tag.transform.ts ***!
  \*************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   addImageTag: () => (/* binding */ addImageTag)
/* harmony export */ });
/* harmony import */ var _constants__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../constants */ 4765);

function addImageTag(_res, options) {
  let {
    fallback
  } = options;
  const {
    pathProp,
    dims,
    sizes,
    srcset
  } = options;
  fallback = fallback || _constants__WEBPACK_IMPORTED_MODULE_0__.POSTER_FALLBACK;
  const res = _res;
  if (sizes !== undefined) {
    res.imgSizes = sizes || '';
    res.imgSrcset = srcset || '';
  }
  res.imgSrc = res[pathProp] ? res[pathProp] + '' : fallback;
  res.imgWidth = dims.WIDTH;
  res.imgHeight = dims.HEIGHT;
  res.imgRatio = res.imgWidth / res.imgHeight;
  return res;
}

/***/ }),

/***/ 746:
/*!********************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/link/a-tag.transform.ts ***!
  \********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   addLinkTag: () => (/* binding */ addLinkTag)
/* harmony export */ });
function addLinkTag(_res, prop, options = {}) {
  let {
    target,
    baseUrl
  } = options;
  target = target || `_blank`;
  baseUrl = baseUrl || `https://www.imdb.com/title/`;
  const res = _res;
  res.href = baseUrl + _res[prop];
  res.target = target;
  return res;
}

/***/ }),

/***/ 9705:
/*!*************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/video/video-tag.transform.ts ***!
  \*************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   addVideoTag: () => (/* binding */ addVideoTag)
/* harmony export */ });
function addVideoTag(_res, options) {
  let {
    baseUrl
  } = options;
  const {
    pathPropFn
  } = options;
  baseUrl = baseUrl || 'https://www.youtube.com/embed';
  const res = _res;
  const path = pathPropFn(res);
  res.videoUrl = path ? `${baseUrl}/${path}` : false;
  return res;
}

/***/ }),

/***/ 7219:
/*!*****************************************************!*\
  !*** ./projects/movies/src/app/state/list.state.ts ***!
  \*****************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ListState: () => (/* binding */ ListState)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! rxjs */ 7835);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! rxjs */ 9877);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 3738);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var _data_access_api_resources_list_resource__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../data-access/api/resources/list.resource */ 5172);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);








class ListState extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__.RxState {
  constructor() {
    (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_2__.DestroyRef).onDestroy(() => this.actionsF.destroy());
    super();
    this.router = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_router__WEBPACK_IMPORTED_MODULE_3__.Router);
    this.actionsF = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__.RxActionFactory();
    this.listResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_data_access_api_resources_list_resource__WEBPACK_IMPORTED_MODULE_0__.ListResource);
    this.actions = this.actionsF.create();
    this.createList = this.actions.createList;
    this.fetchList = this.actions.fetchList;
    this.updateList = this.actions.updateList;
    this.addMovieToList = this.actions.addMovieToList;
    this.deleteMovieFromList = this.actions.deleteMovieFromList;
    this.deleteList = this.actions.deleteList;
    this.deleteListSignal$ = this.actions.deleteList$;
    this.sideEffects$ = (0,rxjs__WEBPACK_IMPORTED_MODULE_5__.merge)(this.actions.addMovieToList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(([movie, id]) => this.listResource.addMovieToList({
      id,
      items: [{
        media_id: movie.id,
        media_type: 'movie'
      }]
    }))), this.actions.deleteMovieFromList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(([movie, id]) => this.listResource.deleteMovieFromList({
      id,
      items: [{
        media_id: movie.id || 0,
        media_type: 'movie'
      }]
    }))), this.actions.updateList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(params => this.listResource.updateList(params))), this.actions.createList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(params => this.listResource.createList(params)), (0,rxjs__WEBPACK_IMPORTED_MODULE_7__.tap)(id => id && this.router.navigate(['account/my-lists']))), this.actions.deleteList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.tap)(id => id && this.router.navigate(['account/my-lists'])), (0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(id => this.listResource.deleteList(id))));
    this.connect('lists', this.actions.fetchList$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.filter)(id => !isNaN(Number(id))), (0,rxjs__WEBPACK_IMPORTED_MODULE_6__.concatMap)(id => this.listResource.fetchList(id))), (state, list) => (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state?.lists || {}, list));
    this.connect('lists', this.actions.updateList$, (state, update) => {
      if (state && update.id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists, {
          [update.id]: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists[update.id], update)
        });
      }
      return state.lists;
    });
    this.connect('lists', this.actions.addMovieToList$, (state, [movie, id]) => {
      if (state && id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists, {
          [id]: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists[id], {
            results: [...(state.lists[id].results || []), movie]
          })
        });
      }
      return state.lists;
    });
    this.connect('lists', this.actions.deleteList$, (state, id) => {
      if (state && id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.deleteProp)(state.lists, `${id}`);
      }
      return state.lists;
    });
    this.connect('lists', this.actions.deleteMovieFromList$, (state, [movie, id]) => {
      if (state && id) {
        return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists, {
          [id]: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(state.lists[id], {
            results: (state.lists[id].results || []).filter(m => m.id !== movie.id)
          })
        });
      }
      return state.lists;
    });
    this.hold(this.sideEffects$);
  }
  initialize() {
    return;
  }
  static #_ = this.ɵfac = function ListState_Factory(t) {
    return new (t || ListState)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineInjectable"]({
    token: ListState,
    factory: ListState.ɵfac,
    providedIn: 'root'
  });
}


/***/ })

}]);
//# sourceMappingURL=default-projects_movies_src_app_pages_account-feature_list-detail-page_list-detail-page_adapter_ts.js.map