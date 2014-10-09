Rails.application.routes.draw do
  namespace :admin do
  get 'domain/index'
  end

  namespace :admin do
  get 'domain/create'
  end

  namespace :admin do
  get 'domain/edit'
  end

  namespace :admin do
  get 'domain/destroy'
  end

  # You can have the root of your site routed with "root"
  root 'home#index'

  get 'home/dev_logged_in' => 'home#dev_logged_in'
  get 'home/wot_logged_in' => 'home#wot_logged_in'
  delete 'home/wot_log_out' => 'home#wot_log_out', :as => :wot_log_out

  get 'home/new_site' => 'home#new_site'
  post 'home/new_site' => 'home#new_site_create'

  namespace 'admin' do
    scope '/:clan_id/' do
      get '/' => 'cp#index'
      resources :forum, :only => [:index, :create, :new, :destroy, :edit, :update]
      resources :acl, :only => [:index]
      resources :newsfeed, :only => [:index, :show, :new, :destroy, :edit, :create]
      resources :alliance, :only => [:index, :create, :show, :new, :destroy]
      # resources :domain, :only => [:index, :edit]
      get 'domain' => 'domain#index' #, :as => :admin_domain_index
      get 'domain/custom/edit' => 'domain#custom_domain_edit' #, :as => :admin_domain_edit
      get 'domain/free/edit' => 'domain#free_domain_edit' #, :as => :admin_domain_edit
    end
  end

  post '/clan_search' => 'home#clan_search'
  scope '/clan/:clan_id/', :constraints => { :clan_id => /\d+/ } do
    get '/' => 'home#clan_index', :as => :clan_index
    resources :forum, :only => [:index, :show]
    scope '/forum/:forum_id/' do
      resources :thread, :only => [:index, :new, :show, :create]
      scope '/thread/:thread_id/' do
        resources :post, :only => [:create, :new]
      end
    end
  end

  # Example of regular route:
  #   get 'products/:id' => 'catalog#view'

  # Example of named route that can be invoked with purchase_url(id: product.id)
  #   get 'products/:id/purchase' => 'catalog#purchase', as: :purchase

  # Example resource route (maps HTTP verbs to controller actions automatically):
  #   resources :products

  # Example resource route with options:
  #   resources :products do
  #     member do
  #       get 'short'
  #       post 'toggle'
  #     end
  #
  #     collection do
  #       get 'sold'
  #     end
  #   end

  # Example resource route with sub-resources:
  #   resources :products do
  #     resources :comments, :sales
  #     resource :seller
  #   end

  # Example resource route with more complex sub-resources:
  #   resources :products do
  #     resources :comments
  #     resources :sales do
  #       get 'recent', on: :collection
  #     end
  #   end

  # Example resource route with concerns:
  #   concern :toggleable do
  #     post 'toggle'
  #   end
  #   resources :post, concerns: :toggleable
  #   resources :photos, concerns: :toggleable

  # Example resource route within a namespace:
  #   namespace :cp do
  #     # Directs /cp/products/* to Admin::ProductsController
  #     # (app/controllers/cp/products_controller.rb)
  #     resources :products
  #   end
end
