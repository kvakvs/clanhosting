class ThreadController < ApplicationController
  def index
    # @forums = Forum::list(session[:user_clan])
  end

  def show
    # clan_id = session[:user_clan]
    # forum_id = params[:id]
    # @forum = Forum.read(clan_id, forum_id)
    # @threads = ForumThread.list(clan_id, forum_id)
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

    fields = {:title => params[:title],
              :body => params[:body] }
    ForumThread.create(session[:user_clan], params[:forum_id], fields)
    redirect_to forum_index_path(:clan_id => session[:user_clan],
                                 :forum_id => params[:forum_id])
  end
end
