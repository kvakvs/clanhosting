class Admin::ForumController < ApplicationController
  include ApplicationHelper

  def index
    return unless require_acl('manage_forums') or require_clan_admin

    @forums = Forum.list(session[:user_clan])
  end

  def new
    return unless require_acl('manage_forums') or require_clan_admin
    true
  end

  def create
    return unless require_acl('manage_forums') or require_clan_admin
    return redirect_to new_admin_forum_path,
                       :alert => t('cp.forums.fill_at_least_title') if params[:title]==''
    fields = {:title => params[:title],
              :desc => params[:desc] || '' }
    Forum::create(session[:user_clan], fields)
    redirect_to admin_forum_index_path
  end

  def destroy
    return unless require_acl('manage_forums') or require_clan_admin
    Forum.delete(session[:user_clan], params[:id])
    redirect_to admin_forum_index_path, :notice => t('cp.forums.deleted')
  end

  def edit
    @forum = Forum.read(session[:user_clan], params[:id])
  end

  # def index
  #   @forums = Forum::list(session[:user_clan])
  # end
  #
  # def show
  #   @threads = ForumThread::list(session[:user_clan], params[:forum_id])
  # end
end
