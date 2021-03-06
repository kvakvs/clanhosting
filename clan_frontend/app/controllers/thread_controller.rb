require 'bb-ruby'
require 'action_view/helpers/date_helper'

class ThreadController < ApplicationController
  include PostHelper

  def index
    # @forums = ForumModel::list(session[:user_clan])
  end

  def show
    clan_id = session[:user_clan]
    forum_id = params[:forum_id]
    thread_id = params[:id]
    @vars = {}
    @vars[:now] = DateTime.now()
    @vars[:thread] = ThreadModel.read_one(clan_id, forum_id, thread_id)
    return redirect_to(forum_path) unless @vars[:thread]
    @vars[:posts]  = PostModel.list(clan_id, thread_id)
  end

  def create
    # TODO: Check clan id, check alliance id
    #return unless require_acl('manage_forums') or require_clan_admin

    new_form = new_thread_path(:clan_id => session[:user_clan],
                               :forum_id => params[:forum_id])

    limit = 5
    return redirect_to new_thread_path(:title => params[:title],
                                       :body => params[:body]),
               :alert => t('app.forums.title_empty',
                           :limit => limit) if params[:title].length < limit
    return redirect_to new_thread_path(:title => params[:title],
                                       :body => params[:body]),
               :alert => t('app.forums.body_empty',
                           :limit => limit) if params[:body].length < limit

    forum_fields = {:title => params[:title],
                    :clan_id => session[:user_clan],
                    :forum_id => params[:forum_id],
                    :created_by => session[:user_account] }
    thread_id = ThreadModel.create(session[:user_clan],
                                   params[:forum_id],
                                   forum_fields)
    # TODO: call post_controller.create instead
    post_fields = {:body => params[:body].bbcode_to_html,
                   :title => params[:title],
                   :clan_id => session[:user_clan],
                   :thread_id => thread_id,
                   :created_by => session[:user_account] }
    PostModel.create(session[:user_clan],
                             thread_id,
                             post_fields)
    redirect_to forum_path(:clan_id => session[:user_clan],
                           :id => params[:forum_id])
  end
end
