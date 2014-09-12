class Admin::NewsfeedController < ApplicationController
  include ApplicationHelper
  before_action :pre_check_site_exists!

  def index
    return unless require_acl('post_news') or require_clan_admin
    @news = NewsfeedItem::all_for_clan(session[:user_clan])
  end

  def new
    return unless require_acl('post_news') or require_clan_admin
    true
  end

  def edit
    return unless require_acl('post_news') or require_clan_admin
    @newsitem = NewsfeedItem.read_one(session[:user_clan], params[:id])
  end

  def destroy
    return unless require_acl('post_news') or require_clan_admin

    NewsfeedItem::delete(session[:user_clan], params[:id])
    redirect_to admin_newsfeed_index_path, :notice => t('cp.newsfeed.deleted')
  end

  def create
    return unless require_acl('post_news') or require_clan_admin

    return redirect_to new_admin_newsfeed_path,
                       :alert => t('cp.newsfeed.fill_at_least_short') if params[:short]==''
    fields = {:title => params[:title],
              :short => params[:short],
              :long  => params[:long] }
    NewsfeedItem::create(session[:user_clan], fields)
    redirect_to admin_newsfeed_index_path
    end
end
