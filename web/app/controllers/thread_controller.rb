class ThreadController < ApplicationController
  def index
    # @forums = Forum::list(session[:user_clan])
  end

  def show
    clan_id = session[:user_clan]
    forum_id = params[:forum_id]
    thread_id = params[:id]
    @thread = ForumThread.read_one(clan_id, forum_id, thread_id)
    @posts = ForumPost.list(clan_id, thread_id)
  end

  def create
    # TODO: Check clan id, check alliance id
    #return unless require_acl('manage_forums') or require_clan_admin

    new_form = new_thread_path(:clan_id => session[:user_clan],
                               :forum_id => params[:forum_id])
    return redirect_to new_form,
               :alert => t('app.forums.title_empty') if params[:title]==''
    return redirect_to new_form,
               :alert => t('app.forums.body_empty') if params[:body]==''

    forum_fields = {:title => params[:title],
                    :created_by => session[:user_account] }
    thread_id = ForumThread.create(session[:user_clan],
                                   params[:forum_id],
                                   forum_fields)
    post_fields = {:body => params[:body],
                   :title => params[:title],
                   :thread_id => thread_id,
                   :created_by => session[:user_account] }
    ForumPost.create(session[:user_clan],
                             thread_id,
                             post_fields)
    redirect_to forum_index_path(:clan_id => session[:user_clan],
                                 :forum_id => params[:forum_id])
  end
end
